
(in-package :abstracttensor/lang)

(defstruct Range
  (from)
  (to)
  (by))

(defmethod print-object ((obj range) stream)
  (format stream "<Range: from ~a to ~a by ~a>"
	  (range-from obj)
	  (range-to obj)
	  (range-by obj)))

(defstruct Counter
  (val 0 :type fixnum)
  (alu 0 :type fixnum))

(defun read-counter (counter name)
  (declare (type Counter counter)
	   (type symbol name))
  (format nil "~(~a~)_~a" name (slot-value counter name)))

(defun spawn-range (from to by)
  (make-range :from from :to to :by by))


;; Feature Enhancement
;; (op X{Array} Y{Array})
;; -> (dotimes (x ...)
;; ->   (dotimes (y ...)
;; ->       (op ... mitaini suru)))
(defun graph-funcall (counter scope form)
  (flet ((explore (code)
	   (alexandria:flatten (graph-funcall counter scope code)))
	 (getscope (name)
	   (or (gethash name scope)
	       (error "~a is not defined." name))))
    (trivia:ematch form
      ;; Iteration over var
      ;; (dotimes (i var) ..)
      ((list* 'dotimes (list bind count) _)

       ;; binds the iterator variables into bind
       (setf (gethash bind scope) (spawn-range 0 count 1))
       (prog1
	   `(,(aten/engine:make-uop-loop
	       :iters (list (getscope bind))
	       :scope :global)
	     ,@(map 'list #'explore (cddr form))
	     ,(aten/engine:make-uop-endloop
	       :iters (list (getscope bind))
	       :option :none))
	 ;; Unbinds
	 (remhash bind scope)))

      ;; Iteration over single variable
      ;; (for (x from to by)
      ;; ...)
      ((list* 'for (list bind from to by) _)
       (setf (gethash bind scope) (spawn-range from to by))
       (prog1
	   `(,(aten/engine:make-uop-loop
	       :iters (list (getscope bind))
	       :scope :global)
	     ,@(map 'list #'explore (cddr form))
	     ,(aten/engine:make-uop-endloop
	       :iters (list (getscope bind))
	       :option :none))
	 (remhash bind scope)))
      ;; (- a) -> -a
      ((list '- form)
       (error "not ready")
       )
      
      ;; arithmetic
      ((list* (or '+ '- '* '/ '> '>= '< '<= '=) _)
       (let ((car (car form))
	     (args (map 'list #'explore (cdr form))))
	 `(,@args
	   ,(aten/engine:make-uop-alu
	     :x-writes
	     (list
	      (prog1
		  (read-counter counter 'alu)
		(incf (counter-alu counter))))
	     :x-reads
	     (map 'list #'aten/engine:uop->buffer args)
	     :op-type (intern (symbol-name car) "KEYWORD")))))

      ;; A = B, A+=B, A-=B, A*=B, A/=B
      ((list (or 'incf 'decf 'mulcf 'divcf) form1)
       (error "not ready"))
      
      ((list (or 'incf 'decf 'mulcf 'divcf) to what)
       (let ((op (case (car form)
		   (incf '+)
		   (decf '-)
		   (mulcf '*)
		   (divcf '/))))
	 (explore
	  `(setf
	    (aref ,@(cdr to))
	    (,op
	     (aref ,@(cdr to))
	     ,what)))))

      ((list 'setf to what)
       (let ((to   (explore to))
	     (what (explore what)))
	 `(,@what
	   ,@to
	   ,(aten/engine:make-uop-load
	     :x1 (aten/engine:uop->buffer to)
	     :x2 (aten/engine:uop->buffer what)))))

      ;; (if exp then &optional else)

      ((list* 'if cond _)
       (error "not ready")
       )

      ;; progn -> { forms }

      ((list* 'progn _)
       `(,@(map 'list #'explore (cdr form))))

      ;; aref -> A[idx]
      ;; Return: Buffer
      ((list* 'aref (type symbol) _)
       (let* (;;subscripts (map 'list #'explore (cddr form)))
	      (buffer (aten/engine:make-aref-buffer
		       :idx  (cddr form);;subscripts
		       :name (getscope (second form)))))
	 (list
	  (aten/engine:make-uop-load
	   :x1 (prog1
		   (read-counter counter 'val)
		 (incf (counter-val counter)))
	   :x2 buffer))))

      ;; funcall. (car cdr)
      ((list* (type symbol) _)
       (error "not ready")
       )
      ;; number
      ((type number)
       (let ((val (aten/engine:make-const-buffer :value form)))
	 (list
	  (aten/engine:make-uop-load
	   :x1 (prog1
		   (read-counter counter 'val)
		 (incf (counter-val counter)))
	   :x2 val))))

      ;; variable
      ((type symbol)
       (let ((val (aten/engine:make-const-buffer :value (getscope (car form)))))
	 (list
	  (aten/engine:make-uop-load
	   :x1 (prog1
		   (read-counter counter 'val)
		 (incf (counter-val counter)))
	   :x2 val))))
      
      ;; string
      ;;((type string) (format nil "\"~a\"" form))

      (_
       (warn "metalize-form: Cannot deal with this form: ~a" form)
       (format nil "~a" form)))))

(defun trace-uops (inputs body &aux (scope (make-hash-table)) (counter (make-counter)))
  "Creates a computation graph from lisp-like DSL"
  (declare (type list inputs body))

  (dolist (input inputs)
    (setf (gethash (intern (symbol-name (aten/ir:aten-id input))) scope) input))
  
  (graph-funcall counter scope body))

