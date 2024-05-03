
;;
;;
;;

(cl:defpackage :clang
  (:use :cl))

(cl:in-package :clang)

;;
;; - 1. SLEEFが使えるかでSIMDifyの分岐をしたい。
;;

;;  DType, op-typeについては規格を作る (OK)
;;  Type Generic
;;  Stride computation
;;  Graph simplification

;; TODO: Export with-clang-runtime
(aten/engine:declare-runtime
 :clang
 :debug 4
 :indexing-rule :flatten ;; manually computes the strides
 )

(defparameter *headers* "
#include <math.h>
")

(defparameter *indent* 0)
(defun indent () (with-output-to-string (out) (dotimes (i *indent*) (princ " " out))))

(defun cName (object)
  (format nil "~(~a~)" object))

(defun uop->line (stream uop)
  (flet ((->buffer (object)
	   ;; [TODO] kokono bunki kirenisuru
	   (if (stringp object)
	       (cName object)
	       (if (symbolp object)
		   (cName object)
		   (if (numberp object)
		       object
		       (aten/engine:render-buffer :clang object))))))
    (aten/engine:uopcase
     uop
     :defun
     ((inputs outputs named)
      (incf *indent* 4)
      (let ((args
	      (apply
	       #'concatenate
	       'string
	       (butlast
		(loop for arg in inputs
		      for scalar-p = (null (aten/ir:aten-shape arg))
		      for type     = (aten/ir:aten-type-class arg)
		      for output-p = (not (find (aten/ir:aten-id arg) outputs :test #'equal))
		      append (list
			      (if output-p
				  "const "
				  "")
			      (cName type)
			      (if scalar-p
				  ""
				  " *")
			      (if output-p
				  (if scalar-p
				      " "
				      " restrict ")
				  " ")
			      (cName (aten/ir:aten-id arg))
			      ", "))))))
	(format
	 stream
	 "~a~%void ~a(~a);~%void ~a(~a) {~%"
	 *headers*
	 named
	 args
	 named
	 args)))
     :enddefun
     ((named)
      (declare (ignore named))
      (decf *indent* 4)
      (format stream "~a} // EndDefun~%" (indent)))     
     :loop
     ((iter scope)
      (let ((id   (aten/engine:range-id   iter))
	    (from (aten/engine:range-from iter))
	    (to   (aten/engine:range-to   iter))
	    (by   (aten/engine:range-by   iter)))
	(format stream "~afor (int ~a=~a; ~a<~a; ~a+=~a) {~%"
		(indent) (->buffer id) (->buffer from) (->buffer id) (->buffer to) (->buffer id) (->buffer by))
	(incf *indent* 4)))
     :endloop
     ((iter option)
      (decf *indent* 4)
      (format stream "~a} // EndLoop ~%" (indent)))
     :load
     ((x1 x2)
      (multiple-value-bind (type pointer-p)
	  (aten/engine:infer-buffer-type x2)
	(format stream "~a~(~a~) ~a~a = ~a; // UOp.Load~%" (indent) type (if pointer-p "*" "") (->buffer x1) (->buffer x2))))
     :alu
     ((x-writes x-reads op-type dtype)
      (cond
	((eql op-type :muladd)
	 ;; out = A * B + C;
	 (format stream "~a~a ~a = ~a * ~a + ~a;~%"
		 (indent)
		 (cName dtype)
		 (->buffer (nth 0 x-writes))
		 (->buffer (nth 0 x-reads))
		 (->buffer (nth 1 x-reads))
		 (->buffer (nth 2 x-reads))))
	((find op-type '(:+ :- :* :/ :< :> :<= :>= :=))
	 ;; Arithmetic
	 (format stream "~a~a ~a = (~a);~%"
		 (indent)
		 (cName dtype)
		 (->buffer (car x-writes))
		 (apply
		  #'concatenate
		  'string
		  (butlast
		   (loop for arg in x-reads
			 for arg-buff = (->buffer arg)
			 append
			 (list arg-buff (format nil "~a" op-type)))))))
	(T
	 (error "Not implemented fcall: ~a" op-type)
	 )))
     :store
     ((x1 x2)
      (format stream "~a~a = ~a; // UOp.Store~%" (indent) (->buffer x1) (->buffer x2))))))

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
	   (declare (ignore type pointer))
	   (typecase value
	     (Aten/IR:AbstractTensor
	      (assert (= 0 (length (aten/ir:aten-shape value))) () "~a is not a scalar." value)
	      (format nil "~a" (cName (aten/ir:aten-id value))))
	     (T
	      (format nil "~a" (cName value)))))
   :aref ((name subscripts)
	  ;; name       = AbstractTensor
	  ;; subscripts = a list of symbols
	  (format nil "~a[~a]"
		  (cName (aten/ir:aten-id name))
		  (apply
		   #'concatenate
		   'string
		   (butlast
		    (loop for s in subscripts
			  append (list s ", "))))))))

