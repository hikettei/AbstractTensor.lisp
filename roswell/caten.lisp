

(cl:in-package :cl-user)

(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(cl:push (cl:pathname "./") ql:*local-project-directories*)
  #+quicklisp(ql:quickload '(:abstracttensor :clingon :cl-ansi-text) :silent t))

(defpackage :caten
  (:use :cl :cl-ansi-text)
  (:export :main))

(in-package :caten)

(defparameter *use-ansi-color* t)
(defmacro maybe-ansi (op &rest args)
  `(if *use-ansi-color*
       (,op ,@args)
       ,@args))

(defun timestamp ()
  (multiple-value-bind
        (second minute hour day month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignore day-of-week dst-p))

    (maybe-ansi
     blue
     (format nil "[~2,'0d:~2,'0d:~2,'0d, ~d/~2,'0d/~d (GMT~@d)]"
	     hour
	     minute
	     second
	     month
	     day
	     year
	     (- tz)))))

(defun print-info (content)
  (format t "~a : ~a~%" (timestamp) content))

(defun parse-test-config (config)
  (let ((args (uiop:split-string config :separator ",")))
    (loop for arg in args
	  for parsed = (uiop:split-string arg :separator "=")
	  collect (cons (symbol-name (read-from-string (car parsed))) (read-from-string (second parsed))))))
#+(or)(print (parse-test-config "M=1, K=2"))

(defun run-compilation (path)
  (let* ((composite (aten/lang:composite-from-file path))
	 (uops      (aten/lang:trace-uops
		     (aten/ir:composite-inputs composite)
		     (read-from-string (aten/ir:composite-code composite))))
	 
	 (graph     (or
		     (aten/engine::with-debug-level (1)
		       (format t "[Compilation Time]~%")
		       (time (aten/engine:uops-optimize uops)))
		     (aten/engine:uops-optimize uops))))
    (multiple-value-bind (ccomposite code)
	(aten/engine:realize graph composite)
      (declare (ignore code))
      (aten/engine::with-debug-level (4)
	(print ccomposite)
	(print graph))
      ccomposite)))

(defun caten/compile (cmd)
  (let* ((path    (clingon:getopt cmd :input))
	 (test    (clingon:getopt cmd :test)))
    (let ((cc (run-compilation path)))
      (when test
	(let ((configs (parse-test-config test)))
	  (multiple-value-bind (errors time)
	      (aten/engine:test-composite cc :constants configs)
	    (format t "[Benchmarks]~%    Error=~a~%    Time=~a~%" errors time)))))))

(defun caten/test (cmd)
  (declare (ignore cmd))
  (print-info "Running test harness...")
  (asdf:test-system "abstracttensor")
  (print-info "Completed"))

(defun caten/handler (cmd &aux (*use-ansi-color* (clingon:getopt cmd :ansi-color t)))
  (macrolet ((of (x) `(equalp *mode* ,x)))

    ;; Configurations
    (load (or (clingon:getopt cmd :runtime) (error "Provide a valid runtime: --runtime <filespec>")))
    (let ((config (parse-test-config (clingon:getopt cmd :config)))
	  (debug  (clingon:getopt cmd :debug 0)))
      (aten/engine:initialize-runtime (aten/engine::runtimeconfig-name aten/engine::*runtime*) config)
      (setf (aten/engine::runtimeconfig-debug aten/engine::*runtime*) debug))
    
    (cond
      ((of "compile")
       (caten/compile cmd))
      ((of "test")
       (caten/test    cmd))
      (T
       (clingon:print-usage *app* t)))))

(defun caten/options ()
  (list
   (clingon:make-option
    :string
    :description "Target toml file to compile"
    :short-name #\i
    :long-name "input"
    :key :input)
   (clingon:make-option
    :string
    :description "Set .lisp file to use as a runtime."
    :short-name #\r
    :long-name "runtime"
    :key :runtime)
   (clingon:make-option
    :integer
    :description "Debug Level (0~4)"
    :short-name #\d
    :long-name "debug"
    :key :debug)
   (clingon:make-option
    :boolean
    :description "Enables/Disables the cl-ansi-color"
    :short-name #\a
    :long-name "ansi-color"
    :key :ansi-color)
   (clingon:make-option
    :string
    :description "Set scalar constants to test the compiled code. (e.g.: --test \"M=1, K=2, N=3\")"
    :short-name #\t
    :long-name "test"
    :key :test)
   (clingon:make-option
    :string
    :description "additional arguments passed to each runtime. (e.g.: --config OpenMP=1,Arm=0)"
    :short-name #\c
    :long-name "config"
    :key :config)))

(defun caten/command ()
  ;; Usage (WIP)
  ;; ./caten -i gemm.toml --disassemble --benchmark i=1,j=1,k=1
  (clingon:make-command
   :name "caten"
   :description "Compile + AbstractTENsor.lisp. (CLI Tools for AbstractTensor.lisp)"
   :authors '("hikettei <ichndm@gmail.com>")
   :license "MIT"
   :options (caten/options)
   :usage "[ compile | test ] [-i <input_file>] [options]"
   :handler #'caten/handler))

(defparameter *mode* "")
(defparameter *app* nil)
(defun main (&rest argv)
  (let ((app (caten/command)))
    (if (= (length argv) 0)
	(clingon:print-usage app t)
	(let ((*mode* (car argv))
	      (*app*  app))
	  (clingon:run app argv)))))

