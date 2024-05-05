

(cl:in-package :cl-user)

(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(cl:push (cl:pathname "./") ql:*local-project-directories*)
  #+quicklisp(ql:quickload '(:abstracttensor :clingon) :silent t))

(defpackage :caten
  (:use :cl)
  (:export :main))

(in-package :caten)

(defun parse-test-config (config)
  (let ((args (uiop:split-string config :separator ",")))
    (loop for arg in args
	  for parsed = (uiop:split-string arg :separator "=")
	  collect (cons (symbol-name (read-from-string (car parsed))) (read-from-string (second parsed))))))
#+(or)(print (parse-test-config "M=1, K=2"))

(defun run-compilation (path runtime debug)
  
  ;; Runtime Configuration

  ;; [TODO] If runtime not found, search from ./runtimes
  (load runtime)

  (setf (aten/engine::runtimeconfig-debug aten/engine::*runtime*) debug)
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
      (aten/engine::with-debug-level (2)
	(format t "[Compiled Code]~%")
	(format t "~a~%" code))
      (aten/engine::with-debug-level (4)
	(print ccomposite)
	(print graph))
      ccomposite)))

(defun caten/handler (cmd)
  (let* ((path (clingon:getopt cmd :input))
	 (runtime (clingon:getopt cmd :runtime))
	 (debug   (or (clingon:getopt cmd :debug) 0))
	 (test    (clingon:getopt cmd :test)))
    (let ((cc (run-compilation path runtime debug)))
      (when test
	(let ((configs (parse-test-config test)))
	  (multiple-value-bind (errors time)
	      (aten/engine:test-composite cc :constants configs)
	    (format t "[Benchmark]~%    Error=~a~%    Time=~a~%" errors time)))))))

(defun caten/options ()
  (list
   (clingon:make-option
    :string
    :description "toml file to compile"
    :short-name #\i
    :long-name "input"
    :key :input)
   (clingon:make-option
    :string
    :description "a .lisp file to use as a runtime"
    :short-name #\r
    :long-name "runtime"
    :key :runtime)
   (clingon:make-option
    :integer
    :description "debug level (0~4)"
    :short-name #\d
    :long-name "debug"
    :key :debug)
   (clingon:make-option
    :string
    :description "tests the compiled composite. (--test \"M=1, K=2, N=3\")"
    :short-name #\t
    :long-name "test"
    :key :test)))

(defun caten/command ()
  ;; Usage (WIP)
  ;; ./caten -i gemm.toml --disassemble --benchmark i=1,j=1,k=1
  (clingon:make-command
   :name "caten"
   :description "Command Line Tool for AbstractTensor.lisp"
   :authors '("hikettei <ichndm@gmail.com>")
   :license "MIT"
   :options (caten/options)
   :usage "[-i <input_file>] [options]"
   :handler #'caten/handler))

(defun main (&rest argv)
  (let ((app (caten/command)))
    (if (= (length argv) 0)
	(clingon:print-usage app t)
	(clingon:run app argv))))

