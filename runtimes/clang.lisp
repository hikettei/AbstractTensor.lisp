
;;
;;
;;

(cl:defpackage :clang
  (:use :cl))

(cl:in-package :clang)

;;
;; - 1. SLEEFが使えるかでSIMDifyの分岐をしたい。
;; - 2. 複雑なIndexを扱う時，Scalar/Scalarの抽象化をしておく。
;; - 3. forのなかでも以下略
;; 

(aten/engine:declare-runtime :clang)

(defun uop->line (stream uop)
  (flet ((->buffer (object)
	   ;; [TODO] kokono bunki kirenisuru
	   (if (stringp object)
	       object
	       (if (symbolp object)
		   object
		   (if (numberp object)
		       object
		       (aten/engine:render-buffer :clang object))))))
    (aten/engine:uopcase
     uop
     :loop
     ((iter scope)
      (let ((id   (aten/engine:range-id iter))
	    (from (aten/engine:range-from iter))
	    (to   (aten/engine:range-to iter))
	    (by   (aten/engine:range-by iter)))
	(format stream "for (int ~a=~a; ~a<~a; ~a+=~a) {~%"
		(->buffer id) (->buffer from) (->buffer id) (->buffer to) (->buffer id) (->buffer by))))
     :endloop
     ((iter option)
      (format stream "}~%"))
     :load
     ((x1 x2)
      (format stream "~a = ~a;~%" (->buffer x1) (->buffer x2)))
     :alu
     ((x-writes x-reads op-type)
      (cond
	((eql op-type :muladd)
	 ;; out = A * B + C;
	 (format stream "~a = ~a * ~a + ~a;~%"
		 (->buffer (nth 0 x-writes))
		 (->buffer (nth 0 x-reads))
		 (->buffer (nth 1 x-reads))
		 (->buffer (nth 2 x-reads))))
	((find op-type '(:+ :- :* :/ :< :> :<= :>= :=))
	 ;; Arithmetic
	 (format stream "~a = ~a(~a);"
		 (->buffer (car x-writes))
		 op-type
		 (map 'list #'->buffer x-reads)))
	(T
	 )))
     :store
     ((x1 x2)
      (format stream "~a = ~a;~%" (->buffer x1) (->buffer x2))))))

(defmethod aten/engine:render-graph ((backend (eql :clang)) uopgraph)
  (with-output-to-string (out)
    (let ((indent 0))
      (declare (ignore indent))
      (dolist (op (aten/engine:uopgraph-uops uopgraph))
	(uop->line out op)))))

(defmethod aten/engine:render-buffer ((backend (eql :clang)) buffer)
  (aten/engine:buffercase
   buffer
   :const ((value type pointer)
	   (format nil "~a ~a ~a" value type pointer))
   :aref ((name subscripts)
	  (format nil "~a[~a]" name subscripts))))

