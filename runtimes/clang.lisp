
;;
;;
;;

(ql:quickload :cffi :silent t)
(cl:defpackage :clang
  (:use :cl :cffi))

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
 :indexing-rule :flatten ;; manually computes the strides
 :scoping-type :static
 :vectorize-strategy :vector
 )
;;#include <x86intrin.h>
;;#include <arm_neon.h>
;;#include <omp.h>
;;#include <sleef.h>
(defparameter *headers* "
#include <stdio.h>
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
		       (format nil "~a" object)
		       (aten/engine:render-buffer :clang object))))))
    (aten/engine:uopcase
     uop
     :declare-var ((var) (declare (ignore var)))
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
			      (if t; output-p
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
     ((iter)
      (let ((id   (aten/engine:range-id   iter))
	    (from (aten/engine:range-from iter))
	    (to   (aten/engine:range-to   iter))
	    (by   (aten/engine:range-by   iter)))
	(format stream "~afor (~a; ~a<~a; ~a+=~a) {~%"
		(indent)
		(if (equal id from) ;; ignore int nth=nth
		    (format nil "")
		    (format nil "int ~a=~a" (->buffer id) (->buffer from)))
		(->buffer id) (->buffer to) (->buffer id) (->buffer by))
	(incf *indent* 4)))
     :endloop
     ((iter)
      (decf *indent* 4)
      (format stream "~a};// EndLoop [~a] ~%" (indent) (aten/engine:range-id iter)))
     :load
     ((x1 x2 reduction)
      (multiple-value-bind (type pointer-p)
	  (aten/engine:infer-buffer-type x2)
	(format stream "~a~(~a~) ~a~a = ~a; // UOp.Load~%" (indent) type (if pointer-p "*" "") (->buffer x1) (->buffer x2))))
     :alu
     ((x-writes x-reads op-type dtype reduction)
      (cond
	((eql op-type :muladd)
	 ;; out = A * B + C;
	 (format stream "~a~a~a = ~a * ~a + ~a;~%"
		 (indent)
		 (if reduction
		     ""
		     (format nil "~a " (cName dtype)))
		 (->buffer (nth 0 x-writes))
		 (->buffer (nth 0 x-reads))
		 (->buffer (nth 1 x-reads))
		 (->buffer (nth 2 x-reads))))
	((find op-type '(:+ :- :* :/ :< :> :<= :>= := :mod :floordiv))
	 ;; Arithmetic
	 (let ((op-c (case op-type
		       (:mod "%")
		       (:floordiv "/") ;; assume out_dtype is properly set to integer.
		       (T op-type))))
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
			   (list arg-buff (format nil "~a" op-c))))))))
	(T
	 (case op-type
	   (T
	    (error "Not implemented fcall: ~a" op-type))))))
     :store
     ((x1 x2 reduction)
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

(defun load-foreign-function (source &key (compiler "gcc") (lang "c") (compiler-flags))
  (declare (type string source compiler))
  (uiop:with-temporary-file (:pathname sharedlib :type "so" :keep t)
    nil
    :close-stream
    (let* ((cmd
	     ;; gcc -shared -o sharedlib
	     (append
	      (list
	       compiler "-shared"
	       "-x" lang)
	      compiler-flags
	      (list "-o" (uiop:native-namestring sharedlib) "-")))
	   (process-info (uiop:launch-program
			  cmd
			  :input :stream
			  :error-output :stream))
	   (input (uiop:process-info-input process-info))
	   (error-output (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source input)
	(close input))
      (unless (zerop (uiop:wait-process process-info))
	(error "AbstractTensor.lisp[clang]: Failed to compile a shared library:~%~a~%

Compiled with: ~a"
	       (alexandria:read-stream-content-into-string error-output)
	       (with-output-to-string (out)
		 (dolist (c cmd) (princ c out) (princ " " out))))))
    (cffi:load-foreign-library sharedlib)))


(defun make-cffi-call-form (name inputs &aux (inames (map 'list #'(lambda (x) (intern (symbol-name (aten/ir:aten-id x)))) inputs)))
  (labels ((expand (rest-forms rest-binds body)
	     (if rest-forms
		 (if (null (aten/ir:aten-shape (car rest-forms)))
		     `(let ((,(car rest-binds) ,(car rest-binds)))
			,(expand (cdr rest-forms) (cdr rest-binds) body))
		     `(with-pointer-to-vector-data (,(car rest-binds) (array-displacement ,(car rest-binds)))
			,(expand (cdr rest-forms) (cdr rest-binds) body)))
		 `(progn ,@body))))
    `(lambda (,@inames)
       ,(expand inputs inames
		`((cffi:foreign-funcall
		   ,name
		   ,@(loop for bind in inames
			   for tensor in inputs
			   for scalar-p = (null (aten/ir:aten-shape tensor))
			   for type     = (aten/ir:aten-type-class tensor)
			   if scalar-p
			     append
			   `(,type ,bind)
			   else
			     append
			   `(:pointer ,bind))
		   :void))))))

(defmethod aten/engine:load-compiled-composite ((backend (eql :clang))
						compiled-code
						composite
						header-object)
  (declare (type string compiled-code)
	   (type aten/engine::UOp-Defun header-object))

  (with-slots ((inputs aten/engine::inputs) (outputs aten/engine::outputs) (named aten/engine::named)) header-object
    (load-foreign-function compiled-code :compiler "gcc")
    ;; inputs = a list of abstracttensor
    
    (aten/engine:make-compiled-composite (compile nil (make-cffi-call-form named inputs)) composite header-object)))
