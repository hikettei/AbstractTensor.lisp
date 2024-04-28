

(cl:in-package :cl-user)

(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(cl:push (cl:pathname "./") ql:*local-project-directories*)
  #+quicklisp(ql:quickload '(:abstracttensor :clingon) :silent t))

(defpackage :caten
  (:use :cl)
  (:export :main))

(in-package :caten)

(defun caten/handler (cmd)
  (let* ((path (clingon:getopt cmd :input))
	 (composite (aten/lang:composite-from-file path)))
    (print
     (aten/lang:trace-uops
      (aten/ir:composite-inputs composite)
      (read-from-string (aten/ir:composite-code composite))))))

(defun caten/options ()
  (list
   (clingon:make-option
    :string
    :description "toml file to compile"
    :short-name #\i
    :long-name "input"
    :key :input)))

(defun caten/command ()
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
