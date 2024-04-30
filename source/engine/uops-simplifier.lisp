
;; Simplifies the sequence of UOps

(in-package :abstracttensor/engine)

(defparameter *simplifiers* (make-hash-table))

(defmacro define-simplifier (name (uops) &body body)
  "
## [macro] define-simplifier

Defines a simplifier which rewrites the DAG graph based on the rule.
TODO:
- Docs
- define-simplifierにドキュメントの機能をつける+自動生成
"
  `(setf (gethash ',name *simplifiers*) #'(lambda (,uops) ,@body)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; (A * B) + C -> MulADD(A, B, C)
(define-simplifier MulAdd (uops)
  (let ((add (second uops))
	(mul (first  uops)))
    (and
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
	   :op-type :muladd))
	 (nthcdr 2 uops)))))))

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
      (uops-simplify uops)
      uops))

(defun %uopgraph-simplify (graph)
  (declare (type UOpGraph graph))
  (setf (uopgraph-uops graph) (uops-simplify (uopgraph-uops graph))))

