
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine.test
  (:use :cl :rove))

(in-package :abstracttensor/engine.test)

(defun uops-from-lisp (inputs code)
  (aten/lang::trace-uops
   (map 'list #'aten/ir::parse-aten inputs)
   (read-from-string code)))

