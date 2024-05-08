
;;
;;
;;

(ql:quickload :cffi :silent t)
(cl:defpackage :clang
  (:use :cl :cffi))

(cl:in-package :clang)

;; Declares compilation strategy
(aten/engine:declare-runtime
 :clang
 :indexing-rule      :flatten
 :scoping-type       :static
 :vectorize-strategy :disabled
 )

;; Memo https://developer.arm.com/architectures/instruction-sets/intrinsics/#f:@navigationhierarchiesreturnbasetype=[int]&f:@navigationhierarchieselementbitsize=[64]&f:@navigationhierarchiessimdisa=[Neon]&q=vld1q_s64

;; TODO: ArmNeonのSIMDの命令, レジスタサイズベースで自動生成

(defun prefix-p (prefix name)
  (if (>= (length name) (length prefix))
      (string= prefix (subseq name 0 (length prefix)))
      nil))

;; ~~ Compilation Options ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defparameter *headers* "")
(defparameter *sleef-p* nil)
(defparameter *omp-p*   nil)
(defparameter *arm-neon-p* nil)
(defparameter *simd-len* nil)
(defparameter *opt* 3)
(defparameter *cc* "gcc")
(defparameter *march* "native")

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod aten/engine:initialize-runtime ((backend (eql :clang)) config)
  (loop for (key . value) in config do
    (cond
      ((equal key "SLEEF")
       (assert (and (numberp value) (or (= value 0) (= value 1)))
	       ()
	       "[Clang] the config SLEEF must be 0 or 1")
       (when (= value 1)
	 (aten/engine::with-debug-level (2)
	   (format t "[Clang] SLEEF is enabled.%"))
	 (setf *sleef-p* 1)
	 (setf *headers* (format nil "~a~%#include <sleef.h>" *headers*))))
      ((equal key "OMP")
       (assert (and (numberp value) (or (= value 0) (= value 1)))
	       ()
	       "[Clang] the config OMP must be 0 or 1")
       (when (= value 1)
	 (aten/engine::with-debug-level (2)
	   (format t "[Clang] OMP is enabled.%"))
	 (setf *headers* (format nil "~a~%#include <omp.h>" *headers*))))
      ((equal key "ARM_NEON")
       (assert (and (numberp value) (or (= value 0) (= value 1)))
	       ()
	       "[Clang] the config ARM_NEON must be 0 or 1")
       (when (= value 1)
	 (aten/engine::with-debug-level (2)
	   (format t "[Clang] ARM_NEON is enabled.%"))
	 (setf *arm-neon-p* 1)
	 (setf *headers* (format nil "~a~%#include <arm_neon.h>" *headers*))))
      ((equal key "SIMD_LEN")
       (assert (integerp value) () "[Clang] SIMD_LEN must be given as an integer. ~a" value)
       (aten/engine::with-debug-level (2)
	 (format t "[Clang] SIMD_LEN=~a~%" value))
       (setf *simd-len* value))
      ((equal key "OPT")
       (assert (typep value '(integer 0 3)) () "[Clang] OPT must be given as an integer from 0 to 3. ~a" value)

       (aten/engine::with-debug-level (2)
	 (format t "[Clang] OPT=~a~%" value))
       (setf *opt* value))
      ((equal key "CC")
       (assert (stringp value) () "[Clang] CC must be given as string. ~a" value)
       (aten/engine::with-debug-level (2)
	 (format t "[Clang] CC=~a~%" value))
       (setf *cc* value))
      ((equal key "MARCH")
       (assert (stringp value) () "[Clang] MARCH must be given as string. ~a" value)
       (aten/engine::with-debug-level (2)
	 (format t "[Clang] MARCH=~a~%" value))
       (setf *march* value))
      (T
       (error "[Clang] Unknown key: ~a -> ~a" key value))))
  
  (flet ((set-simd-id (n)
	   (setf (aten/engine::runtimeconfig-simd-len aten/engine::*runtime*) n)))

    ;; Allows vectorization
    (when (or *sleef-p* *arm-neon-p*)
      (when  *simd-len*
	(error "Cannot enable auto vectorization because SIMD_LEN is not declared.")
	(setf (aten/engine::runtimeconfig-vectorize-strategy aten/engine::*runtime*) :vector)
	(set-simd-id *simd-len*)))))

;; ~~ SIMD Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun packed-dtype (dtype)
  (ecase dtype
    (:float
     (cond
       (*arm-neon-p* "float32x4_t")
       (T (error "fell through"))))
    (:double
     (cond
       (*arm-neon-p* "float64x2_t")
       (T (error "fell through"))))
    (:int
     (cond
       (*arm-neon-p* "int32x4_t")
       (T (error "fell through"))))))

(defun aten-simd-pack-array (dtype)
  (ecase dtype
    (:float
     (cond
       (*arm-neon-p* "vld1q_f32")
       (T (error "fell through"))))
    (:int
     (cond
       (*arm-neon-p* "vld1q_s64")
       (T (error "fell through"))))))

(defun aten-simd-unpack (dtype)
  (ecase dtype
    (:float
     (cond
       (*arm-neon-p* "vst1q_f32")
       (T (error "fell through"))))
    (:int
     (cond
       (*arm-neon-p* "vst1q_s64")
       (T (error ""))))))

(defun aten-simd-op (dtype op)
  (ecase op
    (:+
     (ecase dtype
       (:double)
       (:float
	(cond
	  (*arm-neon-p* )))))
    (:*
     (ecase dtype
       (:double)
       (:float
	(cond
	  (*arm-neon-p* )))))
    (:muladd
     (ecase dtype
       (:double)
       (:float)))))

;; ~~ Codegen Engine ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defparameter *indent* 0)
(defun indent () (with-output-to-string (out) (dotimes (i *indent*) (princ " " out))))

(defun cName (object)
  (format nil "~(~a~)" object))

(defun cType (type)
  (ecase type
    (:double "double")
    (:float "float")
    (:int "int64_t")))

(defun uop->line (stream uop)
  (flet ((->buffer (object)
	   (if (numberp object)
	       (format nil "~a" object)
	       (aten/engine:render-buffer :clang object))))
    (aten/engine:uopcase
     uop
     :declare-var ((var dtype pp) (declare (ignore var dtype pp)))
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
	(let ((simd-p (or (aten/engine::packed-buffer-p x2) (prefix-p "__packed" x1))))
	  (format stream "~a~(~a~) ~a~a = ~a; // UOp.Load~%"
		  (indent)
		  (if simd-p
		      (packed-dtype type)
		      type)
		  (if (and (null simd-p) pointer-p) "*" "")
		  (->buffer x1) (->buffer x2)))))
     :alu
     ((x-writes x-reads op-type dtype reduction)
      (let ((simd-p (some #'aten/engine::packed-buffer-p x-reads)))
	(cond
	  ((eql op-type :muladd)
	   ;; out = A * B + C;
	   (format stream "~a~a~a = ~a * ~a + ~a;~%"
		   (indent)
		   (if reduction
		       ""
		       (if simd-p
			   (format nil "~a " (packed-dtype dtype))	 
			   (format nil "~a " (cName dtype))))
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
	      (error "Not implemented fcall: ~a" op-type)))))))
     :store
     ((x1 x2 reduction)
      (format stream "~a~a = ~a; // UOp.Store~%" (indent) (->buffer x1) (->buffer x2))))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defmethod aten/engine:render-graph ((backend (eql :clang)) uopgraph)
  (with-output-to-string (out)
    (let ((indent 0))
      (declare (ignore indent))
      (dolist (op (aten/engine:uopgraph-uops uopgraph))
	(uop->line out op)))))

(defmethod aten/engine:render-buffer ((backend (eql :clang)) buffer)
  (when (numberp buffer)
    (return-from aten/engine:render-buffer (format nil "~a" buffer)))
  
  (aten/engine:buffercase
   buffer
   :string ((value) (cName value))
   :packed ((packed-objects type)
	    (format nil "~a(((const ~a[]){~a}))"
		    (aten-simd-pack-array type)		    
		    (cType type)
		    (apply
		     #'concatenate
		     'string
		     (butlast
		      (loop for obj in packed-objects
			    append (list (aten/engine:render-buffer backend obj)  ", "))))))
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
			  append (list (aten/engine:render-buffer backend s) ", "))))))))

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
    (load-foreign-function
     compiled-code
     :compiler *cc*
     :compiler-flags
     (append
      (list
       "-fPIC"
       (format nil "-O~a" *opt*)
       (format nil "-march=~a" *march*))
      
      (when *arm-neon-p*
	(list "-mfpu=neon" "-ftree-vectorize"))))
    ;; inputs = a list of abstracttensor
    
    (aten/engine:make-compiled-composite (compile nil (make-cffi-call-form named inputs)) composite header-object)))
