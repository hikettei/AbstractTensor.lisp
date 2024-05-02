
;; Simplifies the sequence of UOps

(in-package :abstracttensor/engine)

(defparameter *simplifiers* (make-hash-table))

(defun remove-uops (uops target)
  (loop for u in uops
	unless (equal u target)
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
	    ;;do (print "+++") (print u) (print (recursively-find-deps uops (uop->buffer u)))
	    do (dolist (w writes) (push w seen))
	    if (seen-p reads) do
	      (push u new-uops)
	    else do
	      (push (cons reads u) stashed)
	    do (loop for (reads-old . u-old) in stashed
		     if (seen-p reads-old)
		       do (push u-old new-uops)
			  (setf stashed (remove u-old stashed :key #'cdr :test #'equal)))))

    ;; TODO: It is ok to exist isolated graphs; since they have no deps relocated to the top of the dag graph.
    ;; (when (not (= (length uops) (length new-uops)))
    ;;   (warn "resolve-isolated-uops: these UOPs are isolated from the DAG and removed:~%~a~%If your compiled code will not work well, that could be due to a bug of simplifiers." stashed))
    
    `(,@(map 'list #'cdr stashed) ,@(reverse new-uops))))

(defmacro define-simplifier (name (uops) &body body)
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
  `(setf (gethash ',name *simplifiers*) #'(lambda (,uops) (block ,name ,@body))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; (A * B) + C -> MulADD(A, B, C)
(define-simplifier MulAdd (uops)
  (let* ((mul (first  uops))
	 (add (uops/value->users uops (uop->buffer mul))))
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
	(find (car mul-writes) add-reads :test #'equal)
	(progn
	  (with-debug-level (3)
	    (format t "[Simplifier] Fused: A*B+C -> MulAdd.~%~a~a" add mul))
	  t)
	;; Merge and rewrite uops
	(append
	 (list
	  (make-uop-alu
	   :x-writes add-writes
	   :x-reads  (loop for arg in add-reads
			   if (string= arg (car mul-writes))
			     append mul-reads
			   else
			     append (list arg))
	   :op-type :muladd
	   :dtype (uop-alu-dtype add)))
	 (remove-uops
	  (remove-uops
	   uops
	   add)
	  mul)))))))

(define-simplifier Purge-Isolated-Load (uops)
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
		   (uops/user->values uops wn))
	       where-to-writes))))
      (when (some #'(lambda (x) x) who-depends-on-load?)->failed)
      (with-debug-level (3)
	(format t "[Simplifier] Purged: ~a" load))
      (cdr uops))))

(define-simplifier FoldConstant[Loop] (uops)
  (symbol-macrolet ((->failed (return-from FoldConstant[Loop] nil)))
    (when (not (uop-load-p (car uops)))->failed)
    (let* ((load (car uops))
	   (usrs (uops/value->users uops (uop-load-x1 load)))
	   (trigger      (uop-load-x1 load))
	   (replace-with (uop-load-x2 load))
	   (targets
	     (loop for usr in usrs
		   if (or
		       (uop-loop-p usr)
		       ;;(uop-load-p usr)
		       )
		     collect usr))
	   (changed-p nil))
      (when (not (const-buffer-p replace-with))->failed)
      
      (macrolet ((replace! (accessor &key (wrap 'progn))
		   `(progn
		      (when (equal ,accessor trigger)
			(with-debug-level (3)
			  (format t "[Simplifier] Rewriting ~a -> ~a of ~a~%"
				  ,accessor
				  (,wrap replace-with)
				  tgt))
			(setf ,accessor (,wrap replace-with)
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
	       (replace! (range-from (uop-loop-iters tgt)))
	       (replace! (range-to   (uop-loop-iters tgt)))
	       (replace! (range-by   (uop-loop-iters tgt))))
	      (UOp-Load
	 ;      (let ((aref (uop-load-x2 tgt)))
	;	 (when (aref-buffer-p aref)
	;	   (dotimes (i (length (aref-buffer-idx aref)))
	;	     (replace! (nth i (aref-buffer-idx aref)) :wrap ->aref-idx))))
	       ))))
	(if changed-p uops nil)))))

;; [TODO]
;; Load-Loop Fuse
;; Bijective Fuse A->B->C, A->C


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun apply-simplifier (uops simplifier &aux (changed-p nil))
  (declare (type list uops)
	   (type function simplifier))
  (loop with count = (length uops)
	for i upfrom 0 below count
	do (let ((ref (funcall simplifier (nthcdr i uops))))
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
      uops))

(defun %uopgraph-simplify (graph)
  (declare (type UOpGraph graph))
  (setf (uopgraph-uops graph) (uops-simplify (uopgraph-uops graph))))

