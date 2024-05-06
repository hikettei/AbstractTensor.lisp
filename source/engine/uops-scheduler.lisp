
(in-package :abstracttensor/engine)

(defun %uopgraph-optimize-accumlation (graph &aux (count 0) (applied-p nil))
  "Attributes @reduction pharse"
  (declare (type UOpGraph graph))

  (assert (not (some #'(lambda (x) (and (uop-load-p x) (uop-load-reduction x))) (uopgraph-uops graph)))
	  ()
	  "Assertion failed: %uopgraph-optimize-accumlation can be applied once.")
  
  (with-local-simplifiers
    (define-simplifier Accumlation->Reduction (uops uops-full)
      ;;
      ;; for (...) {
      ;;   Z = Z + a1 * a1
      ;; }
      ;; ->
      ;; float acc_n = 0;
      ;; for (...) {
      ;;   acc_n = a1 * a1
      ;; }
      ;; Z = acc_n

      (symbol-macrolet ((->failed (return-from Accumlation->Reduction nil)))
	(when (not (uop-loop-p (car uops)))->failed)

	;; This simplification can be applied once.
	(when applied-p ->failed)
	(let* ((uop-loop  (car uops))
	       (range-idx (range-id (uop-loop-iters uop-loop)))
	       (end-loop  (find range-idx (cdr uops) :test #'equal :key #'(lambda (x) (and (uop-endloop-p x) (range-id (uop-endloop-iters x)))))))
	       
	  (when (null end-loop)->failed)

	  ;; Only the innermost loops are the subject to this simplification.
	  (loop with ids = nil
		for uop in uops
		if (uop-loop-p uop)
		  do (push (range-id (uop-loop-iters uop)) ids)

		if (uop-endloop-p uop)
		  do (setf ids (remove (range-id (uop-endloop-iters uop)) ids :test #'equal))

		if (> (length ids) 1)
		  do (progn ->failed))
	  
	  ;; a requirement for applying this simplification:
	  ;;  - Innermost Loop
	  ;;  - UOp-Store exists
	  ;;      - it depends on the computation inside the loop
	  ;;      - it does not depends on range-idx

	  (let* ((endloop-position (position end-loop uops :test #'equal))
		 (innerloop-uops  (subseq uops 1 endloop-position))
		 (stores-to-rewrite
		   (loop for uop in innerloop-uops
			 if (uop-store-p uop)
			   collect
			   (let* ((x1 (uop-store-x1 uop))
				  (x2 (uop-store-x2 uop)))
			     (when (aref-buffer-p x1)
			       ;; x1 must be independant from range-idx
			       (let ((parents
				       (loop for idx in (aref-buffer-idx x1)
					     append
					     (recursively-find-deps uops-full idx))))
				 (when (null (find range-idx parents :test #'equal))
				   ;; Finding ALU from parents
				   (let ((values (uops/user->values innerloop-uops x2)))
				     
				     ;; It is asserted that multiply-accumlation is fused by other simplifiers
				     ;; [TODO] More casse for simplification ops
				     ;; currently :muladd is only supported.
				     (when (and
					    (= (length values) 1)
					    (uop-alu-p (car values))
					    (eql :muladd (uop-alu-op-type (car values))))
				       ;; :muladd is used as an accumlation?
				       ;; e.g.: C = A * B + C
				       (multiple-value-bind (a b c)
					   (apply #'values (uop-alu-x-reads (car values)))
					 (declare (ignore a b))
					 (when (equalp c x1)
					   ;; (cons ALU Store)
					   (cons (car values) uop)))))))))))
		 (stores-to-rewrite
		   (loop for x in stores-to-rewrite if x collect x))
		 (accumlators
		   ;; It is asserted that there's no other accumlators
		   ;; so we can count the idx from zero.
		   (loop for i in stores-to-rewrite
			 collect
			 (prog1
			     (format nil "_acc_~a" count)
			   (incf count))))
		 (acc-loaders
		   (loop for (alu . store) in stores-to-rewrite
			 for accumlator in accumlators
			 collect
			 (make-uop-load
			  :x1 accumlator
			  :x2 (uop-store-x1 store)
			  :reduction range-idx)))
		 (acc-store-ops
		   (loop for (alu . store) in stores-to-rewrite
			 for accumlator in accumlators
			 collect
			 (make-uop-store
			  :x1 (uop-store-x1 store)
			  :x2 accumlator
			  :reduction range-idx))))
	    
	    ;; Rewriting Rule:
	    ;; ALU
	    ;;  - alu_n = A * B + C
	    ;;  -> acc_n = A * B + acc_n
	    
	    (loop for (alu . store) in stores-to-rewrite
		  for accumlator in accumlators do
		    (with-debug-level (3)
		      (format t "[Simplifier] Detected Reduction: ~a ~a~%" accumlator alu))
		    (setf uops
			  (replace-uop
			   uops alu
			   (make-uop-alu
			    :x-writes (list accumlator)
			    :x-reads  `(,@(subseq (uop-alu-x-reads alu) 0 2) ,accumlator)
			    :op-type (uop-alu-op-type alu)
			    :dtype (uop-alu-dtype alu)
			    :reduction range-idx))
			  uops
			  (remove-uops uops store)))

	    ;; Adding acc-store-ops
	    (setf uops (alexandria:flatten (replace-uop uops end-loop `(,end-loop ,@acc-store-ops))))
	    (setf applied-p t)
	    `(,@acc-loaders
	      ,@uops)))))
    
    (%uopgraph-simplify graph)
    graph))


;;
;; Utils for manipulating UOp-Loop and UOp-EndLoop
;; including
;;  - Unroll
;;  - Split
;;  - Reduce
;;  et al.
;;

(defun slice-loop-entity (uops idx)
  (declare (type list uops)
	   (type string idx))
  (flet ((idx-reader (type-p read)
	   #'(lambda (x) (and (funcall type-p x) (range-id (funcall read x))))))
    (let ((start (position idx uops :test #'equal :key (idx-reader #'uop-loop-p #'uop-loop-iters)))
	  (end   (position idx uops :test #'equal :key (idx-reader #'uop-endloop-p #'uop-endloop-iters))))
      (when (and start end)
	(assert (< start end) () "Invaild Graph Structure is detected. EndLoop comes before Loop")
	(let* ((end (1+ end))
	       (sliced (subseq uops start end)))
	  (assert (string=
		   (the simple-base-string (range-id (uop-loop-iters (car sliced))))
		   (the simple-base-string (range-id (uop-endloop-iters (car (last sliced))))))
		  ()
		  "Assertion failed")
	  (values sliced start end))))))

(defun unroll-uops (uops unroll-idx unroll-by &aux (seen (list unroll-idx)))
  "
for (int i=0;i<3;i+=2) {
    int k = i;            | <- unroll-uops this area as an `uops`
    ...                   | <- rewriting each ops as unrolled ops
}
"
  ;; [TODO] Unroll
  ;; Load -> Unroll
  ;; ALU -> Unroll
  ;; etc
  (labels ((->unroll-idx (name nth)
	     (if (equal (aref name 0) #\_)
		 (format nil "~a_~a" name nth)
		 (format nil "_~a_~a" name nth)))
	   (to-unroll? (uop)
	     (intersection seen (uop-reads uop) :test #'equal))
	   (unroll-buffer? (name)
	     (find name seen :test #'equal))
	   (unroll-buffer (buffer nth)
	     (if (typep buffer 'buffers)
		 (buffercase
		  buffer
		  :string ((value)
			   (if (unroll-buffer? value)
			       (->unroll-idx value nth)
			       value))
		  :const  ((value type pointer-p)
			   (if (unroll-buffer? value)
			       (make-const-buffer :value (->unroll-idx value nth) :type type :pointer-p pointer-p)
			       value))
		  :aref ((name idx)
			 (make-aref-buffer
			  :name name
			  :idx
			  (map
			   'list
			   #'(lambda (x)
			       (if (unroll-buffer? x)
				   (->unroll-idx x nth)
				   x))
			   idx))))
		 buffer)))
    
    (alexandria:flatten
     (list
      (loop for nth upfrom 0 below unroll-by
	    collect
	    (make-uop-alu
	     :x-writes (list (->unroll-idx unroll-idx nth))
	     :x-reads  (list unroll-idx nth)
	     :op-type :+
	     :dtype   :int))
      (loop for op in uops
	    if (to-unroll? op)
	      collect
	      (typecase op
		(UOp-Load
		 ;; int val_0 = i;
		 ;; ->
		 ;; int val_0_1 = i+0;
		 ;; int val_0_2 = i+1;
		 ;;       ...
		 (push (uop-load-x1 op) seen)
		 (loop for nth upfrom 0 below unroll-by
		       if (equal (uop-load-reduction op) unroll-idx)
			 collect op
		       else
			 collect
			 (make-uop-load
			  :x1 (unroll-buffer (uop-load-x1 op) nth)
			  :x2 (unroll-buffer (uop-load-x2 op) nth)
			  :reduction (uop-load-reduction op))))
		(UOp-Store
		 (loop for nth upfrom 0 below unroll-by
		       if (equal (uop-store-reduction op) unroll-idx)
			 collect op
		       else
			 collect
			 (make-uop-store
			  :x1 (unroll-buffer (uop-store-x1 op) nth)
			  :x2 (unroll-buffer (uop-store-x2 op) nth)
			  :reduction (uop-store-reduction op))))
		(UOp-ALU
		 (dolist (w (uop-alu-x-writes op))
		   (push w seen))
		 (loop with writes = (uop-alu-x-writes op)
		       with reduction-p = (equal unroll-idx (uop-alu-reduction op))
		       for nth upfrom 0 below unroll-by
		       collect
		       (make-uop-alu
			:x-writes (if (equal unroll-idx (uop-alu-reduction op))
				      (uop-alu-x-writes op)
				      (map 'list #'(lambda (x) (unroll-buffer x nth)) (uop-alu-x-writes op)))
			:x-reads  (map
				   'list
				   #'(lambda (x)
				       (if (and reduction-p (find x writes :test #'equal))
					   x
					   (unroll-buffer x nth)))
				   (uop-alu-x-reads  op))
			:op-type  (uop-alu-op-type op)
			:dtype    (uop-alu-dtype op)
			:reduction (uop-alu-reduction op))))
		(UOp-Loop
		 (progn;;let ((loop-subbody (slice-loop-entity uops (range-id (uop-loop-iters op)))))
		   ;; TODO
		   ;; ループごと繰り返すように実装したい。(がテストするのがめんどくさい) Conv実装する時にでも。。。
		   (error "not ready!")
		   ))
		(T
		 (error "unroll-uops: add the case for unrolling ~a" op)
		 op))
	    else
	      collect op)))))

(defun %uopgraph-unroll (graph idx unroll-by scope-type &key (unroller #'unroll-uops))
  (declare (type UOpGraph graph)
	   (type string idx)
	   (type fixnum unroll-by)
	   (type function unroller)
	   (type (member :dynamic :static) scope-type)
	   (optimize (speed 3)))

  (multiple-value-bind (loop-body start end) (slice-loop-entity (uopgraph-uops graph) idx)
    (assert loop-body () "Loop(~a) is not defined." idx)
    (let* ((loop-start (car loop-body))
	   (old-range (uop-loop-iters loop-start))
	   (loop-fixed?
	     (and
	      (numberp (range-from old-range))
	      (numberp (range-by   old-range))
	      (numberp (range-to   old-range))))
	   (1by1?
	     (equal 1 (range-by old-range))))

      (when (not 1by1?)
	(with-debug-level (3)
	  (warn "%uopgraph-unroll is skipped because the `by` of the range ~a is not 1." old-range))
	(return-from %uopgraph-unroll))

      (let* ((new-range-to-idx (format nil "_~(~a~)_upper" (range-id old-range)))
	     (new-range-to (make-uop-alu
			    :x-writes (list new-range-to-idx)
			    :x-reads  (list (range-to old-range) (1- unroll-by))
			    :op-type :-
			    :dtype :int))
	     (new-range (make-range
			 :id   (range-id old-range)
			 :from (range-from old-range)
			 :to   new-range-to-idx
			 :by   unroll-by))
	     (n-loopsize-idx (format nil "_len_~(~a~)_~a" (range-id old-range) unroll-by))
	     (n-reminder-idx (format nil "_rem_~(~a~)_~a" (range-id old-range) unroll-by))
	     (n-frm-idx      (format nil "_frm_~(~a~)_~a" (range-id old-range) unroll-by))
	     
	     ;; _len_n_16 = (- (range-to old-range) (range-from old-range))
	     (loopsize (if (and (numberp (range-to old-range)) (numberp (range-from old-range)))
			   (the fixnum (- (the fixnum (range-to old-range)) (the fixnum (range-from old-range))))
			   (make-uop-alu :x-writes (list n-loopsize-idx) :x-reads `(,(range-to old-range) ,(range-from old-range)) :op-type :- :dtype :int)))

	     (loopsize-buffer (or (and (numberp loopsize) loopsize) n-loopsize-idx))
	     ;; _rem_n_16 = n-loopsize-idx // unroll-by
	     (n-reminder-alu (make-uop-alu :x-writes (list n-reminder-idx) :x-reads `(,loopsize-buffer ,unroll-by) :op-type :floordiv :dtype :int))
	     ;; _frm_n_16 = 16 * _n_rem_n_16 + offset
	     (n-frm-size     (make-uop-alu :x-writes (list n-frm-idx) :x-reads `(,unroll-by ,n-reminder-idx ,(range-from old-range)) :op-type :muladd :dtype :int))
	     (reminder-range
	       (if loop-fixed?
		   ;; If the range of iter is fixed (i.e.: to/from/by is a number)
		   ;; first compute whether there is reminder or not.
		   (let* ((loop-size (the fixnum (- (the fixnum (range-to old-range)) (the fixnum (range-from old-range)))))
			  (n-reminder (floor loop-size unroll-by)))
		     (when (< loop-size unroll-by)
		       ;; In this case, the iteration is already the equivalent to loop-reminder parts.
		       (with-debug-level (3)
			 (warn "%uopgraph-unroll is ignored because of (< loop-size unroll-by). loop-size=~a unroll-by=~a" loop-size unroll-by))
		       (return-from %uopgraph-unroll nil))
		     (if (= (mod loop-size unroll-by) 0)
			 nil ;; there is no need to prepare reminder loop.
			 (make-range
			  :id   (range-id old-range)
			  :from (the fixnum (+ (the fixnum (range-from old-range)) (the fixnum (* n-reminder unroll-by))))
			  :to   (range-to old-range)
			  :by 1)))
		   
		   ;; If the range of iter is consisted of dynamic shape
		   ;; we basically renders a loop3 reminder.
		   (make-range
		    :id (range-id old-range)
		    :from n-frm-idx
		    :to  (range-to old-range)
		    :by  1)))
	     (loop-reminder
	       (ecase scope-type
		 (:dynamic
		  ;;slow
		  (with-debug-level (3)
		    (warn "[Unroll] Unrolling ~a with scoping-type=:dynamic" (range-id old-range)))
		  
		  (when reminder-range
		    (append
		     (when (and (not loop-fixed?) (not (numberp loopsize))) (list loopsize))
		     (when (not loop-fixed?)                                (list n-reminder-alu n-frm-size))
		     (list (make-uop-loop :iters reminder-range))
		     (butlast (cdr loop-body))
		     (list (make-uop-endloop :iters reminder-range)))))
		 (:static
		  ;;optimized
		  (when reminder-range
		    (when (not loop-fixed?)
		      (setf (range-from reminder-range) (range-id reminder-range)))
		    (append
		     (list (make-uop-loop :iters reminder-range))
		     (butlast (cdr loop-body))
		     (list (make-uop-endloop :iters reminder-range)))))))	 
	     (unrolled-loop-body
	       (append
		;; When writing loop-reminder and scope-type is static
		;; Remove the part computing simd_end_idx.
		(when (and loop-reminder (eql scope-type :static))
		  (setf (range-from new-range) (range-id new-range))
		  (list (make-uop-load :x1 (range-id old-range) :x2 (make-const-buffer :value (range-from old-range) :type :int))))
		(list (make-uop-loop :iters new-range))
		(funcall unroller (cdr (butlast loop-body)) (range-id old-range) unroll-by)
		(list (make-uop-endloop :iters new-range))))
	     (new-loop-body `(,@unrolled-loop-body ,@loop-reminder))
	     ;; Replace the existing loop with unrolled loop
	     (new-body `(,@(subseq (uopgraph-uops graph) 0 start)
			 ,new-range-to
			 ,@new-loop-body
			 ,@(subseq (uopgraph-uops graph) end))))
	(setf (uopgraph-uops graph) new-body)

	;; [FixME]
	;; With loop_reminder and scope=:static, it brokes the definition of non-circular of DAG.
	;; That is, some assertion of simplifiers may broke?...
	graph))))

