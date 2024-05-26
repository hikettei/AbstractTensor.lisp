
;; Simplifies the sequence of UOps

(in-package :abstracttensor/engine)

(defparameter *simplifiers* (make-hash-table))

(defun remove-uops (uops target)
  (loop for u in uops
	unless (equal u target)
	  collect u))

(defun replace-uop (uops target replace-with)
  (declare (type list uops))
  (loop for u in uops
	if (equal u target)
	  collect replace-with
	else
	  collect u))

(defun resolve-isolated-uops (uops)
  (declare (type list uops))
  (let ((new-uops)
	(seen)
	(stashed))
    (declare (type list new-uops seen stashed))
    (flet ((seen-p (reads)
	     (every #'(lambda (x) (find x seen :test #'equal)) reads)))
      (loop for u in uops
	    for position fixnum upfrom 0
	    for reads  = (uop-reads u)
	    for writes = (uop-writes u)
	    if (seen-p reads) do
	      (dolist (w writes) (push w seen))
	      (push u new-uops)
	    else do
	      (push (cons reads u) stashed)
	    end
	    
	    do (loop with finish-p = nil
		     with changed-p = nil
		     while (not finish-p)
		     do (setf changed-p nil)			
			(loop for (reads-old . u-old) in stashed
			      if (seen-p reads-old)
				do (push u-old new-uops)
				   (setf changed-p t)
				   (dolist (w (uop-writes u-old)) (push w seen))
				   (setf stashed (remove u-old stashed :key #'cdr :test #'equal)))
			(setf finish-p (not changed-p)))))

    ;; TODO: It is ok to exist isolated graphs; since they have no deps relocated to the top of the dag graph.
    (with-debug-level (3)
      (when (not (= (length uops) (length new-uops)))
	(format t "[Simplifier] These nodes are isolated from the graph and purged in the simplification process. ~a" stashed)))
    
    (reverse new-uops)))

(defmacro define-simplifier (name (uops &optional (uops-all (gensym))) &body body)
  "
## [macro] define-simplifier

Defines a simplifier which rewrites the DAG graph based on the rule.
TODO:
- Docs
- define-simplifierにドキュメントの機能をつける+自動生成

Body is a function where:

((x)
 Body)

x is a list of uops. For i = 0...(length uops), x = (nthcdr i uops)
And body:
- Return NIL if the optimization cannot be applied
- Return a rewritten graph if the optimization technique can be applied.
"
  `(progn
     (when (gethash ',name *simplifiers*)
       (warn "Redefining the simplifier ~a~%" ',name))
     (setf (gethash ',name *simplifiers*) #'(lambda (,uops ,uops-all) (declare (ignorable ,uops-all)) (block ,name ,@body)))))

(defmacro with-local-simplifiers (&body body)
  `(let ((*simplifiers* (make-hash-table)))
     ,@body))

;; ~~~ Hand-Coded Simplifiers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; (A * B) + C -> MulADD(A, B, C)
(define-simplifier WMMA-Fusion (uops uops-full)
  (let* ((mul (first  uops))
	 (add (uops/value->users uops-full (uop->buffer mul))))
    (and
     (= (length add) 1)
     (progn
       (setf add (car add))
       t)
     (typep add 'UOp-ALU)
     (typep mul 'UOp-ALU)
     (eql (uop-alu-op-type add) :+)
     (eql (uop-alu-op-type mul) :*)
     (let ((add-reads  (uop-alu-x-reads  add))
	   (add-writes (uop-alu-x-writes add))
	   (mul-reads  (uop-alu-x-reads  mul))
	   (mul-writes (uop-alu-x-writes mul)))
       (and
	(= (length add-reads) (length mul-reads) 2)
	(= (length add-writes) (length mul-writes) 1)
	(equal (uop-alu-reduction mul) (uop-alu-reduction add))
	(find (car mul-writes) add-reads :test #'equal)
	(progn
	  (with-debug-level (3)
	    (format t "[Simplifier] Fused: A*B+C -> WMMA.~%~a~a" add mul))
	  t)
	;; Merge and rewrite uops
	(append
	 (list
	  (make-uop-alu
	   :x-writes add-writes
	   ;; C = A * B + C
	   :x-reads
	   (let ((a (car mul-reads))
		 (b (second mul-reads))
		 (c (loop named find-c
			  for r in add-reads
			  if (not (string= r (car mul-writes)))
			    do (return-from find-c r))))
	     (list a b c))
	   :op-type :wmma
	   :dtype (uop-alu-dtype add)
	   :reduction (or (uop-alu-reduction mul) (uop-alu-reduction add))))
	 (remove-uops
	  (remove-uops
	   uops
	   add)
	  mul)))))))

(define-simplifier Purge-Isolated-Load (uops uops-full)
  (symbol-macrolet ((->failed (return-from Purge-Isolated-Load nil)))
    (when (not (uop-load-p (car uops)))->failed)
    
    (let* ((load (car uops))
	   (where-to-writes (uop-writes load))
	   (who-depends-on-load?
	     (apply
	      #'append
	      (map
	       'list
	       #'(lambda (wn)
		   (uops/value->users uops-full wn))
	       where-to-writes))))

      (when (and who-depends-on-load? (some #'(lambda (x) x) who-depends-on-load?))->failed)
      (with-debug-level (3)
	(format t "[Simplifier] Purged: ~a~%" load))
      (cdr uops))))

(define-simplifier FoldConstant[Loop] (uops uops-full)
  (symbol-macrolet ((->failed (return-from FoldConstant[Loop] nil)))
    (when (not (uop-load-p (car uops)))->failed)
    (let* ((load (car uops))
	   (usrs (uops/value->users uops-full (uop-load-x1 load)))
	   (trigger      (uop-load-x1 load))
	   (replace-with (uop-load-x2 load))
	   (targets
	     (loop for usr in usrs
		   if (or
		       (uop-loop-p usr)
		       ;;(uop-load-p usr)
		       )
		     collect usr))
	   (changed-p nil)
	   (writes (uops/user->values uops-full trigger)))

      ;; Ignore the case of:
      ;; int i_1 = 0
      ;; for(int i_1=i_1; ... ;...)
      ;; (this is intended)      
      (when (not (= (length writes) 1))->failed)
      (when (not (const-buffer-p replace-with))->failed)
      
      (macrolet ((replace! (accessor &key (wrap 'progn))
		   `(progn
		      (when (equal ,accessor trigger)
			(with-debug-level (3)
			  (format t "[Simplifier] Rewriting ~a -> ~a of ~a~%"
				  ,accessor
				  (,wrap replace-with)
				  tgt))
			(setf ,accessor (let ((val (,wrap replace-with)))
					  (if (aten/ir::abstracttensor-p val)
					      (aten/ir::aten-id val)
					      val))
			      changed-p t)))))
	;; [TODO] More
	(labels ((->name (obj)
		   (if (numberp obj)
		       obj
		       (format nil "~(~a~)" obj)))
		 (->aref-idx (obj)
		   (->name (const-buffer-value obj))))
	  (dolist (tgt targets)
	    (typecase tgt
	      (UOp-Loop
	       (replace! (range-from (uop-loop-iters tgt)) :wrap const-buffer-value)
	       (replace! (range-to   (uop-loop-iters tgt)) :wrap const-buffer-value)
	       (replace! (range-by   (uop-loop-iters tgt)) :wrap const-buffer-value))
	      (UOp-Load
	       ;;      (let ((aref (uop-load-x2 tgt)))
	       ;;	 (when (aref-buffer-p aref)
	       ;;	   (dotimes (i (length (aref-buffer-idx aref)))
	       ;;	     (replace! (nth i (aref-buffer-idx aref)) :wrap ->aref-idx))))
	       ))))
	(if changed-p uops nil)))))

(define-simplifier Load-Fusion (uops uops-full)
  (symbol-macrolet ((->failed (return-from Load-Fusion nil)))
    ;;       x1  x2
    ;; Load1 X <- Y
    ;; Load2 Z <- X
    ;;
    ;; Rewrites Load2 with Load Z<-Y
    ;; If this operation made Load1 isolated in the graph, Load1 is expected to be purged.
    (when (not (uop-load-p (car uops)))->failed)
    (let* ((load1 (car uops))
	   (replace-with
	     (if (const-buffer-p (uop-load-x2 load1))
		 (uop-load-x2 load1)
		 ->failed))
	   (load2s (uops/value->users uops-full (uop->buffer load1)))
	   (changed-p nil)
	   (_ (when (not (= (length (uops/user->values uops-full (uop-load-x1 load1))) 1))->failed))
	   (load2s-new
	     (loop for l in load2s
		   if (uop-load-p l)
		     collect (progn
			       (setf changed-p t)
			       (with-debug-level (3)
				 (format t "[Simplifier] LoadFusion ~a -> ~a~%" l replace-with))
			       (make-uop-load
				:x1 (uop-load-x1 l)
				:x2 replace-with
				:reduction (uop-load-reduction l)))
		   else
		     collect l)))
      (declare (ignore _))
      (loop for x in load2s
	    for y in load2s-new
	    do (setf uops (replace-uop uops x y)))
      (when changed-p uops))))

;; Arithmetic Simplification Patterns
(macrolet ((def (name msg pattern)
	     `(define-simplifier ,name (uops uops-full)
		(symbol-macrolet ((->failed (return-from ,name nil)))

		  ;; Finds this pattern in this order: X -> Z -> Y
		  ;;
		  ;; X
		  ;; |   Y1
		  ;;  \ /----Y2 ...
		  ;;   Z
		  ;;
		  ;; pattern specifies the function which rewrites Z

		  (when (not (uop-load-p (car uops)))->failed)
		  (let* ((x  (car uops))
			 (zs (uops/value->users uops (uop->buffer x)))
			 (z  (car zs)))
		    (when (not (= (length zs) 1))->failed)
		    (when (not (uop-alu-p z))->failed)
		    (let* ((ys
			     (loop for a in (uop-reads z)
				   collect
				   (let ((result (uops/user->values uops-full a)))
				     (when (not (= (length result) 1))->failed)
				     (car result)))))
		      (when (not (every #'(lambda (x) (or (uop-load-p x) (uop-alu-p x))) ys))->failed)
		      (let ((replacement (funcall #'(lambda ,@pattern) z ys)))
			(declare (type list replacement))
			;; replacement = replacement for ALU(z)
			
			(when replacement
			  (with-debug-level (3)
			    (format t "~%[Simplifier] ~a ALU[~a] (Replace: ~a ==> ~a)~%"
				    ,msg
				    (uop-alu-op-type z)
				    ys
				    replacement))
			  ;; Unused ys are expected to be purged by another simplifier.
			  ;; Note: replacement == list
			  (alexandria:flatten (replace-uop uops z replacement))))))))))
  
  (def Propagate-Constants
    "Constant Propagation"
    ((alu ys)
     (when (not (find (uop-alu-op-type alu) `(:+ :- :* :/ := :< :<= :> :>= :wmma)))->failed)
     (let* ((new-args
	      (loop for y-old in ys
		    if (uop-load-p y-old)
		      collect
		      (multiple-value-bind (x1 x2)
			  (values (uop-load-x1 y-old)
				  (uop-load-x2 y-old))
			(declare (ignore x1))
			;; Here, allows to use Aref/Const Buffer (1)
			(buffercase
			 x2
			 :const ((value type pointer-p)
				 (declare (ignore value type pointer-p))
				 (typecase (const-buffer-value x2)
				   (number
				    (let ((out (const-buffer-value x2)))
				      (if (eql (uop-alu-op-type alu) :*)
					  (if (= out 1)
					      nil
					      out)
					  out)))
				   (aten/ir:AbstractTensor
				    (assert (null (const-buffer-pointer-p x2)) () "Assertion failed")
				    (aten/ir:aten-id (const-buffer-value x2)))
				   (T (const-buffer-value x2))))
			 :aref ((idx name)
				(declare (ignore idx name))
				x2)
			 ;; Packed Load/Store cannot be fused
			 :packed ((packed-objects dtype) ->failed)
			 :string ((value) (error "not tested this line yet") value)))
		    if (uop-alu-p y-old)
		      collect (car (uop-alu-x-writes y-old))))
	    (new-args (loop for x in new-args if x collect x))
	    (new-args (if (every #'numberp new-args)
			  (case (uop-alu-op-type alu)
			    (:wmma (list (+ (* (nth 0 new-args) (nth 1 new-args)) (nth 2 new-args))))
			    (T     (list (apply (intern (symbol-name (uop-alu-op-type alu))) new-args))))
			  new-args))
	    (new-args (if (and
			   (find (uop-alu-op-type alu) `(:+ :*))
			   (some #'numberp new-args))
			  (loop named accumlator with stashed = nil
				for arg in new-args
				if (numberp arg) do
				  (if (null stashed)
				      (setf stashed arg)
				      (setf stashed (funcall (intern (symbol-name (uop-alu-op-type alu))) stashed arg)))
				finally
				   (return-from
				    accumlator
				     `(,stashed
				       ,@(loop for x in new-args
					       if (not (numberp arg)) collect x))))
			  new-args)))
       (print new-args)
       (cond
	 ((and (every #'numberp new-args) (= (length new-args) 1))
	  (loop with type = (if (uop-load-p (car ys))
				(const-buffer-type (uop-load-x2 (car ys)))
				(if (uop-alu-p (car ys))
				    (uop-alu-dtype (car ys))
				    (error "ConstPropagation: Cannot infer the dtype from ~a" (car ys))))
		for x-write in (uop-alu-x-writes alu)
		collect
		(make-uop-load
		 :x1 x-write
		 :x2 (make-const-buffer
		      :value (car new-args)
		      :type  type
		      :pointer-p nil))))
	 ((eql (uop-alu-op-type alu) :wmma)
	  (trivia:match new-args
	    ((list (type number) (type number) (type symbol))
	     (list
	      (make-uop-alu
	       :x-writes (uop-alu-x-writes alu)
	       :x-reads `(,(* (nth 0 new-args) (nth 1 new-args)) ,(nth 2 new-args))
	       :op-type :+
	       :dtype (uop-alu-dtype alu))))
	    (_
	     (assert (= (length new-args) 3) () "Simplifier: Assertion failed. (new-args is not expected to be fused?)")
	     (list
	      (make-uop-alu
	       :x-writes (uop-alu-x-writes alu)
	       :x-reads new-args
	       :op-type :wmma
	       :dtype (uop-alu-dtype alu))))))
	 ((= (length new-args) 1)
	  (if (find (uop-alu-op-type alu) `(:+ :*))
	      (loop for x-write in (uop-alu-x-writes alu)
		    collect
		    (make-uop-load
		     :x1 x-write
		     ;; (The code is suck but) note that this line corresponds with (1)
		     ;; If you allow another buffer to pass at (1)
		     ;; This line also needs to be changed.
		     :x2 (cond
			   ((aref-buffer-p (car new-args)) (car new-args))
			   (T
			    (make-const-buffer
			     :value (car new-args)
			     :type  (uop-alu-dtype alu)
			     :pointer-p nil)))))
	      nil))
	 (T
	  (list
	   (make-uop-alu
	    :x-writes (uop-alu-x-writes alu)
	    :x-reads  (loop for arg in new-args
			    if (or (numberp arg) (typep arg 'graph-id))
			      collect arg
			    else
			      collect (or (uop->buffer arg) arg))
	    :op-type (uop-alu-op-type alu)
	    :dtype   (uop-alu-dtype alu))))))))

  )

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun apply-simplifier (uops simplifier &aux (changed-p nil))
  (declare (type list uops)
	   (type function simplifier))
  (loop with count = (length uops)
	for i upfrom 0 below count
	do (let ((ref (funcall simplifier (nthcdr i uops) uops)))
	     (when ref
	       (setf changed-p t
		     uops `(,@(subseq uops 0 i) ,@ref)))))
  (values changed-p uops))

(defun uops-simplify (uops &aux (changed nil))
  "
## [function] uops-simplify

Simplifies the uops. In the stage of simplifying the DAG,
the following optimizations are performed iteratively:
- removal of unused graphs
- graph fusion.
"
  (declare (type list uops))
  
  (maphash
   #'(lambda (name simplifier)
       (declare (ignore name))
       (multiple-value-bind (changed-here new-uops)
	   (apply-simplifier uops simplifier)
	 (setf uops new-uops
	       changed (or changed changed-here))))
   *simplifiers*)
  (if changed
      (uops-simplify (resolve-isolated-uops uops))
      (resolve-isolated-uops uops)))

(defun %uopgraph-simplify (graph)
  (declare (type UOpGraph graph))
  (setf (uopgraph-uops graph) (uops-simplify (uopgraph-uops graph))))

