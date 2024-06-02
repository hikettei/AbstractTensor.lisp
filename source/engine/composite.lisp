
(in-package :abstracttensor/engine)

;; Composite (a.k.a: Compiled Function)

(defun find-output-positions (uop-defun)
  (declare (type UOp-Defun uop-defun))
  (let ((outputs (uop-defun-outputs uop-defun)))
    (loop for k in outputs
	  collect (position k (uop-defun-inputs uop-defun) :key (alexandria:compose #'aten/ir:aten-id) :test #'equalp))))

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
      Backend    -> ~a
      Input      -> (~a)
      Outputs    -> ~a
      TestingKit -> ~a
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
	  (uop-defun-outputs (cc-defun-header cc))
	  (if (string= "" (aten/ir:composite-test-code (cc-base-composite cc)))
	      "Unavailable"
	      "Available")))

(defun call (composite &rest args)
  "Funcalls the compiled composite."
  (declare (type Compiled-composite composite))

  ;; As of now, this function is UNSAFE
  ;; [TODO] Add Type check, etc

  (apply (cc-caller composite) args)
  (apply #'values (loop for nth in (cc-output-positions composite)
			collect (nth nth args))))

;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-initializer (f)
  #'(lambda (aten shape)
      (let* ((dp  (make-array (apply #'* shape) :element-type (->lisp-type (aten/ir:aten-type-class aten))))
	     (out (make-array shape :element-type (->lisp-type (aten/ir:aten-type-class aten)) :displaced-to dp)))
	(dotimes (i (apply #'* shape))
	  (setf (aref dp i) (funcall f i)))
	out)))

(defun random-initializer (&rest args)
  (apply (make-initializer #'(lambda (x) (declare (ignore x)) (- 1.0 (random 2.0)))) args))

(defun zero-initializer (&rest args)
  (apply (make-initializer #'(lambda (x) (declare (ignore x)) 0.0)) args))

(defun MSE (x y)
  (declare (type array x y))
  (let ((x1 (array-displacement x))
	(x2 (array-displacement y))
	(sum   0.0)
	(total (array-total-size x)))
    (dotimes (i total)
      (incf sum (expt (- (aref x1 i) (aref x2 i)) 2)))
    (/ sum total)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defparameter *test-composite-mode* nil)
(defun test-composite (compiled-composite
		       &key
			 (constants)
			 (initializer #'random-initializer) ;; buffer for input
			 (allocator   #'zero-initializer)   ;; buffer for output
			 (error-function #'MSE)
			 (use-outputs nil))
  "[TODO] Docs
constants = `((\"I\" . 1) (\"B\" . 2))
return -> (values accuracy time_compiled_composite time_test_code)"
  (declare (type Compiled-Composite compiled-composite)
	   (type list constants))
  ;; [TODO] How to transfer arrays into CUDA?
  ;; [TODO] Memory-Order?
  (when (string= "" (aten/ir:composite-test-code (cc-base-composite compiled-composite)))
    (error "test-composite: Testing Kits for ~a is not loaded." compiled-composite))
  
  (let* ((*test-composite-mode* t)
	 (variables (make-hash-table :test #'equal)))

    (loop for (id1 . value) in constants
	  for id = (if (keywordp id1)
		       (symbol-name id1)
		       id1)
	  do (assert (stringp id) () "test-composite: ~a should be a string." id)
	     (assert (numberp value) () "test-composite: ~a should be a scalar. (or, constants must be given as: `((NAME . VALUE) ...))" value)
	     (setf (gethash (string-upcase id) variables) value))

    (let ((inputs (uop-defun-inputs (cc-defun-header compiled-composite))))
      (flet ((allocate-args (f &optional (common-inputs))
	       (loop for arg in inputs
		     for nth upfrom 0
		     for output-p = (find nth (cc-output-positions compiled-composite) :test #'=)
		     collect
		     (or
		      (when common-inputs
			(if output-p
			    (if (numberp (nth nth common-inputs))
				(nth nth common-inputs)
				(let ((shapes (map 'list #'(lambda (x) (gethash (symbol-name x) variables)) (aten/ir:aten-shape arg))))
				  (funcall allocator arg shapes)))
			    ;; Input
			    (nth nth common-inputs)))
		       (dolist (x (aten/ir:aten-shape arg))
			 (or
			  (find (symbol-name x) (alexandria:hash-table-keys variables) :test #'equal)
			  (error "test-composite: ~a is not provided." x)))
		       (if (aten/ir:aten-shape arg)
			   (let ((shapes (map 'list #'(lambda (x) (gethash (symbol-name x) variables)) (aten/ir:aten-shape arg))))
			     (funcall f arg shapes))
			   (let ((value (gethash (aten/ir:aten-id arg) variables)))
			     (assert value () "test-composite: A scalar tensor ~a is not provided." arg)
			     value))))))

	;; Computing on the compiled composite
	(macrolet ((bench (form)
		     `(let ((t1 (get-internal-real-time)))
			(multiple-value-list ,form)
			(let ((t2 (get-internal-real-time)))
			  (float (/ (- t2 t1) internal-time-units-per-second))))))
	  (let* ((common-inputs (allocate-args initializer))
		 (args-tgt (allocate-args initializer common-inputs))
		 (args-org (allocate-args initializer common-inputs))
		 (tester (compile nil (read-from-string (aten/ir:composite-test-code (cc-base-composite compiled-composite)))))
		 (target-case-time   (bench (apply #'call compiled-composite args-tgt)))
		 (original-case-time (bench (apply tester args-org))))

	    ;; [TODO] Use AbstractTensor.lisp itself to implement MSE Error
	    
	    (values
	     (loop for idx in (cc-output-positions compiled-composite)
		   collect (funcall error-function (nth idx args-tgt) (nth idx args-org)))
	     target-case-time
	     original-case-time
	     (when use-outputs
	       (loop for idx in (cc-output-positions compiled-composite)
		     collect (nth idx args-tgt)))
	     (when use-outputs
	       (loop for idx in (cc-output-positions compiled-composite)
		     collect (nth idx args-org))))))))))

