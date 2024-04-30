

(cl:in-package :cl-user)

(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(cl:push (cl:pathname "./") ql:*local-project-directories*)
  #+quicklisp(ql:quickload '(:abstracttensor :clingon) :silent t))

(defpackage :caten
  (:use :cl)
  (:export :main))

(in-package :caten)

(defun run-compilation (path runtime)
  
  ;; Runtime Configuration
  ;; [TODO] If runtime not found, search from ./runtimes
  (load runtime)
  
  (let* ((composite (aten/lang:composite-from-file path))
	 (uops      (aten/lang:trace-uops
		     (aten/ir:composite-inputs composite)
		     (read-from-string (aten/ir:composite-code composite))))
	 (graph     (aten/engine:uops-optimize (print uops)))
	 (code      (aten/engine:realize graph)))
    (print code)
    (print graph)
    ))

(defun caten/handler (cmd)
  (let* ((path (clingon:getopt cmd :input))
	 (runtime (clingon:getopt cmd :runtime)))
    (run-compilation path runtime)))

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
    :key :runtime)))

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

