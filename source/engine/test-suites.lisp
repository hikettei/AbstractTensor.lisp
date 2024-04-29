
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine.test
  (:use :cl :rove))

(in-package :abstracttensor/engine.test)

(defun uops-from-lisp (inputs code)
  (aten/lang::trace-uops
   (map 'list #'aten/ir::parse-aten inputs)
   (read-from-string code)))

(defmacro define-simplify-test ((&rest inputs) code ((uops simplified-uop) &body body))
  `(let* ((,uops (uops-from-lisp ',inputs ,code))
	  (,simplified-uop (aten/engine:uops-simplify ,uops)))
     ,@body))

;; Mul, Add -> MulAdd
(define-simplify-test ("A{T}[i j]<1 0>()" "B{T}[j k]<1 0>()" "C{T}[i k]<1 0>()")
		      "
(dotimes (i1 256)
    (dotimes (j1 256)
        (dotimes (k1 256)
	    (setf (aref c i1 k1) 0))
        (dotimes (k1 256)
	    (incf (aref c i1 k1) (* (aref a i1 j1) (aref b j1 k1))))))
"
    ((base new)
      (flet ((find-alu (key)
	       (find key new :key #'(lambda (x) (if (aten/engine::uop-alu-p x) (aten/engine::uop-alu-op-type x) nil)))))
	(ok
	 (and (find-alu :muladd) (not (find-alu :+)) (not (find-alu :*)))))))

