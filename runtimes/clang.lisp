
;;
;;
;;

(cl:defpackage :clang
  (:use :cl))

(cl:in-package :clang)

(aten/engine:declare-runtime :clang)

(defun uop->line (stream uop)
  (aten/engine:uopcase
   uop
   :loop
   ((iter scope)
    (let ((id   (aten/engine:range-id iter))
	  (from (aten/engine:range-from iter))
	  (to   (aten/engine:range-to iter))
	  (by   (aten/engine:range-by iter)))
      (format stream "for (int ~a=~a; ~a<~a; ~a+=~a) {~%"
	      id from id to id by)))
   :endloop
   ((iter option)
    (format stream "}~%"))
   :load
   ((x1 x2)
    (format stream "~a = ~a;~%" x1 x2))
   :alu
   ((x-writes x-reads op-type)
    (format stream "~a ~a ~a;~%" x-writes op-type x-reads))
   :store
   ((x1 x2)
    (format stream "~a = ~a;~%" x1 x2))))

(defmethod aten/engine:render ((backend (eql :clang)) uopgraph)
  (with-output-to-string (out)
    (let ((indent 0))
      (declare (ignore indent))
      (dolist (op (aten/engine:uopgraph-uops uopgraph))
	(uop->line out op)))))


