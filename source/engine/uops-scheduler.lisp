
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
			     (format nil "acc_~a" count)
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
  (flet ((idx-reader (type-p read)
	   #'(lambda (x) (and (funcall type-p x) (range-id (funcall read x))))))
    (let ((start (position idx uops :test #'equal :key (idx-reader #'uop-loop-p #'uop-loop-iters)))
	  (end   (position idx uops :test #'equal :key (idx-reader #'uop-endloop-p #'uop-endloop-iters))))
      (assert (< start end) () "Invaild Graph Structure is detected. EndLoop comes before Loop")
      (values (subseq uops start end) start end))))

(defun unroll-uop (uop unroll-idx unroll-by)
  ;; Load -> Unroll
  ;; ALU -> Unroll
  ;; etc
  )

(defun %uopgraph-unroll (graph idx unroll-by)
  (declare (type UOpGraph graph)
	   (type string idx)
	   (type fixnum unroll-by))
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

      (let* ((new-range (make-range
			 :id   (range-id old-range)
			 :from (range-from old-range)
			 :to   (range-to   old-range)
			 :by   unroll-by))
	     (n-reminder-idx (format nil "_mod_~(~a~)_~a" (range-id old-range) unroll-by))
	     (n-loopsize-idx (format nil "_len_~(~a~)_~a" (range-id old-range) unroll-by))

	     ;; _len_n_16 = (- (range-to old-range) (range-from old-range))
	     (loopsize       (if (and (numberp (range-to old-range)) (numberp (range-from old-range)))
				 (- (range-to old-range) (range-from old-range))
				 (make-uop-alu :x-writes (list n-loopsize-idx) :x-reads `(,(range-to old-range) ,(range-from old-range)) :op-type :- :dtype :int)))

	     ;; _mod_n_16 = n-loopsize-idx // unroll-by
	     (n-reminder-alu (make-uop-alu :x-writes (list n-reminder-idx) :x-reads `(,loopsize ,unroll-by) :op-type :mod :dtype :int))
	     (reminder-range
	       (if loop-fixed?
		   ;; If the range of iter is fixed (i.e.: to/from/by is a number)
		   ;; first compute whether there is reminder or not.
		   (let ((n-reminder (mod (- (range-to old-range) (range-from old-range)) (range-by old-range))))
		     (if (= n-reminder 0)
			 nil ;; there is no need to prepare reminder loop.
			 (make-range
			  :id   (range-id old-range)
			  :from (range-to old-range)
			  :to   n-reminder
			  :by 1)))
		   
		   ;; If the range of iter is consisted of dynamic shape
		   ;; we basically renders a loop3 reminder.
		   (make-range
		    :id (range-id old-range)
		    :from (range-to old-range)
		    :to n-reminder-idx
		    :by 1)))
	     (loop-reminder
	       (when reminder-range
		 (append
		  (when (and (not loop-fixed?) (not (numberp loopsize))) (list loopsize))
		  (when (not loop-fixed?)                                (list n-reminder-alu))
		  (list (make-uop-loop :iters reminder-range))
		  (butlast (cdr loop-body))
		  (list (make-uop-endloop :iters reminder-range)))))
	     (unrolled-loop-body
	       (append
		(list (make-uop-loop :iters new-range))
		(alexandria:flatten (map 'list #'(lambda (x) (unroll-uop x (range-id old-range) unroll-by)) (cdr (butlast loop-body))))
		(list (make-uop-endloop :iters new-range))))
	     (new-loop-body `(,@unrolled-loop-body ,@loop-reminder))
	     (new-body `(,@(subseq (uopgraph-uops graph) 0 start)
			 ,@new-loop-body
			 ,@(subseq (uopgraph-uops graph) end))))
	(setf (uopgraph-uops graph) new-body)))))
	
	
	
