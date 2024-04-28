
;; Simplifies the sequence of UOps

(in-package :abstracttensor/engine)

(defparameter *simplifiers* (make-hash-table))

(defmacro define-simplifier (name &key (trigger) (rewriter))
  `(progn
     (setf (gethash ',name *simplifiers*)
	   (cons #'(lambda ,@trigger) #'(lambda ,@rewriter)))))

(define-simplifier MulAdd
  :trigger
  ((uops)
   (let ((add (first  uops))
	 (mul (second uops)))
     (and
      (typep add 'UOps-ALU)
      (typep mul 'UOps-ALU)
      (eql (uop-alu-op-type add) :+)
      (eql (uop-alu-op-type mul) :*)
      ;;; todo
      ;; first, write a better graph tracing!!
      
      )))
  :rewriter
  ((uops)
   ))

(define-simplifier Fold-Constant
  :trigger
  ((uops)
   )
  :rewriter
  ((uops)
   ))


(defun apply-simplifier (uops trigger rewriter &aux (changed-p nil))
  (declare (type list uops)
	   (type function trigger rewriter))
  (loop with count = (length uops)
	for i upfrom 0 below count
	do (if (funcall trigger (nthcdr i uops))
	       (setf changed-p t
		     uops `(,@(subseq uops 0 count) ,@(funcall rewriter (nthcdr i uops))))))		 
  (values changed-p uops))

(defun uops-simplify (uops &aux (changed nil))
  (declare (type list uops))

  (maphash
   #'(lambda (name ops)
       (declare (ignore name))
       (multiple-value-bind (changed-here new-uops)
	   (apply-simplifier uops (car ops) (cdr ops))
	 (setf uops new-uops
	       changed (or changed changed-here))))
   *simplifiers*)
  (if changed
      (uops-simplify uops)
      uops))
