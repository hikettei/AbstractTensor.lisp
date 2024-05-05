
(in-package :abstracttensor/engine)

;; Composite (a.k.a: Compiled Function)

(defun find-output-positions (uop-defun)
  (declare (type UOp-Defun uop-defun))
  (let ((outputs (uop-defun-outputs uop-defun)))
    (loop for o in outputs
	  for k = (symbol-name o)
	  collect (position k (uop-defun-inputs uop-defun) :key (alexandria:compose #'symbol-name #'aten/ir:aten-id) :test #'equal))))

(defstruct (Compiled-Composite
	    (:conc-name cc-)
	    (:constructor
		make-compiled-composite
		(caller composite defun-header)))
  "TODO: Docs"
  (runtime (runtimeconfig-name *runtime*) :type keyword)
  (base-composite composite :type aten/ir:Composite)
  (caller caller :type function)
  (defun-header defun-header :type UOp-Defun)
  (output-positions (find-output-positions defun-header) :type list))

(defmethod print-object ((cc Compiled-Composite) stream)
  (format stream "<Compiled-Composite[~a]
    [~a]
      Backend -> ~a
      Input   -> (~a)
      Outputs -> ~a
>
"
	  (uop-defun-named (cc-defun-header cc))
	  (aten/ir:composite-name (cc-base-composite cc))
	  (cc-runtime cc)
	  (apply
	   #'concatenate
	   'string
	   (butlast
	    (loop for arg in (uop-defun-inputs (cc-defun-header cc))
		  for scalar-p = (null (aten/ir:aten-shape arg))
		  for type     = (aten/ir:aten-type-class arg)
		  for shape    = (aten/ir:aten-shape arg)
		  append
		  (list
		   (if scalar-p
		       ""
		       "*")
		   (format nil "~(~a~)" type)
		   " "
		   (format nil "~(~a~)" (aten/ir:aten-id arg))
		   (when shape
		     (format nil "[~(~a~)]" shape))
		   ", "))))
	  (uop-defun-outputs (cc-defun-header cc))))

(defun call (composite &rest args)
  "Funcalls the compiled composite."
  (declare (type Compiled-composite composite))

  ;; As of now, this function is UNSAFE
  ;; [TODO] Add Type check, etc

  (apply (cc-caller composite) args)
  (apply #'values (loop for nth in (cc-output-positions composite)
			collect (nth nth args))))


