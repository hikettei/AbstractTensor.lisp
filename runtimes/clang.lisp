
;;
;;
;;

(cl:defpackage :clang
  (:use :cl))

(cl:in-package :clang)

(aten/engine:declare-runtime :clang)

(defun uop->line (uop)
  (aten/engine:uopcase
   uop
   :load ((x1 x2)
	  (format nil "~a = ~a;~%" x1 x2))))

(defmethod aten/engine:render ((backend (eql :clang)) uopgraph)
  
  )

