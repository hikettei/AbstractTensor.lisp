
(in-package :abstracttensor/engine)

;; TODO: DebugsしやすくするためにRendererを書いてみる

(defparameter *runtime* nil
  "## [parameter] *runtime*
Set RuntimeConfig to use.")

(defstruct (RuntimeConfig
	    (:constructor make-runtime (name
					&key
					  (debug 0)
					  (group-for-reduces 1)
					  (upcasted 1)
					  (dont-use-locals nil))))
  (name name :type keyword)
  (debug debug :type (integer 0 4))
  (group-for-reduces group-for-reduces :type fixnum)
  (upcasted upcasted :type fixnum)
  (dont-use-locals dont-use-locals :type boolean))

(defmacro declare-runtime (runtime-id &rest args)
  "[TODO] Docs"
  `(setf *runtime* (make-runtime ,runtime-id ,@args)))

(defgeneric render-graph (backend uop-graph)
  (:documentation "[TODO] Renders the uop-graph"))

(defgeneric render-buffer (backend buffer)
  (:documentation "[TODO]"))

(defun realize (uop-graph composite &key (function-name (symbol-name (gensym "KID"))))
  "[TODO] Doc finsih the complitaion."
  (declare (type UOpGraph uop-graph))

  (flet ((->make-const (scalar)
	   (aten/ir::make-aten scalar :int nil nil nil)))
    (let ((new-uop-graph (copy-UOpGraph uop-graph))
	  ;; Gathering dynamic shapes
	  (dynamic-shapes
	    (remove-duplicates
	     (loop for i in (aten/ir:composite-inputs composite)
		   append
		   (loop for s in (aten/ir:aten-shape i)
			 if (not (numberp s))
			   collect s)))))
      (setf (UOpGraph-uops new-uop-graph)
	    `(,(aten/engine:make-uop-defun
		:inputs
		`(,@(aten/ir:composite-inputs composite)
		  ,@(map 'list #'->make-const dynamic-shapes))
		:outputs (aten/ir:composite-outputs composite)
		:named function-name)
	      ,@(UOpGraph-uops new-uop-graph)
	      ,(aten/engine:make-uop-enddefun
		:named function-name)))
      (render-graph
       (runtimeconfig-name *runtime*)
       new-uop-graph))))


;; Utils
(defun infer-buffer-type (buffer)
  "Infers the type of buffer.
Return: (values type-keyword pointer-p)"
  (declare (type buffers buffer))

  (when (stringp buffer)
    (error "Cannot infer the type of ~a." buffer))

  (buffercase
   buffer
   :const
   ((value type pointer-p)
    (declare (ignore value))
    (values type pointer-p))
   :aref
   ((name idx)
    (declare (ignore idx))
    (values (aten/ir:aten-type-class name) nil))))
