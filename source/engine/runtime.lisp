
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

(defun realize (uop-graph)
  "[TODO] Doc finsih the complitaion."
  (declare (type UOpGraph uop-graph))
  (render-graph
   (runtimeconfig-name *runtime*)
   uop-graph))
