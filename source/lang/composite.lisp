
(in-package :abstracttensor/lang)

(defun cName (string)
  (cl-ppcre:regex-replace-all "-" string "_"))

(defmacro with-text ((bind path) &body body)
  `(let ((,bind (alexandria:read-file-into-string ,path)))
     ,@body))

(defun composite-from-toml (toml)
  (declare (type string toml))
  (let ((config (clop:parse toml :style :alist)))
    (flet ((config-of (from name &optional optional)
	     (or (let ((out (find name from :key #'car :test #'string=)))
		   (and out (cdr out)))
		 (if optional
		     optional
		     (error "~a not found from config: ~a" name config)))))
      ;;(print config)
      (let ((composite (config-of config "composite"))
	    (impl      (config-of config "implementation"))
	    (test      (config-of config "test" :none)))
	(aten/ir:make-composite
	 :documentation (config-of composite "documentation" "")
	 :path ""
	 :name (config-of composite "name" (gensym "CID"))
	 :inputs (map 'list #'aten/ir:parse-aten (config-of composite "inputs"))
	 :outputs (map 'list #'(lambda (x) (cName (string-upcase x))) (config-of composite "outputs"))
	 :code (config-of impl "code")
	 :test-requirements (if (eql test :none)
				nil
				(let ((out (config-of test "requirements" :none)))
				  (if (eql out :none) nil out)))
	 :test-code (if (eql test :none)
			""
			(config-of test "code" "")))))))

(defun composite-from-file (filepath)
  (with-text (toml filepath)
    (let ((result (composite-from-toml toml)))
      (dolist (o (aten/ir:composite-outputs result))
	(assert (find o (aten/ir:composite-inputs result) :key #'aten/ir:aten-id :test #'equalp)
		()
		"The output ~a is not appeared in the declared ShapeTracker: ~a"
		o
		(aten/ir:composite-inputs result)))
      (setf (aten/ir:composite-path result) filepath)
      result)))
