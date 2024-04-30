
(in-package :abstracttensor/lang)

(defstruct Counter
  "gensym alternative"
  (val 0 :type fixnum)
  (alu 0 :type fixnum))

(defun read-counter (counter name)
  (declare (type Counter counter)
	   (type symbol name))
  (format nil "~(~a~)_~a" name (slot-value counter name)))

(defun spawn-range (id from to by)
  (make-range :id id :from from :to to :by by))


;; TODO: Replace it with Elegant BNF Parser
;; Feature Enhancement
;; (op X{Array} Y{Array})
;; -> (dotimes (x ...)
;; ->   (dotimes (y ...)
;; ->       (op ... mitaini suru)))
;; - TODO: Imrpove the quality of error message
(defun graph-funcall (counter scope form)
  (flet ((explore (code)
	   (graph-funcall counter scope code))
	 (getscope (name)
	   (or (gethash name scope)
	       (error "~a is not defined." name))))
    (trivia:ematch form
      ;; Iteration over var
      ;; (dotimes (i var) ..)
      ((list* 'dotimes (list bind count) _)

       ;; binds the iterator variables into bind
       (let* ((count (explore count))
	      (count-idx (aten/engine:uop->buffer (car (last count))))
	      (range (spawn-range bind 0 count-idx 1)))
	 (setf (gethash bind scope) range)
	 (prog1
	     `(,@count
	       ,(aten/engine:make-uop-loop
		 :iters range
		 :scope :global)
	       ,@(apply #'append (map 'list #'explore (cddr form)))
	       ,(aten/engine:make-uop-endloop
		 :iters range
		 :option :none))
	   ;; Unbinds
	   (remhash bind scope))))

      ;; Iteration over single variable
      ;; (for (x from to by)
      ;; ...)
      ;; TODO: Update
      ((list* 'for (list bind from to by) _)
       (let* ((from-uop (explore from))
	      (to-uop   (explore to))
	      (by-uop   (explore by))
	      (from-buff (aten/engine:uop->buffer (car (last from-uop))))
	      (to-buff   (aten/engine:uop->buffer (car (last to-uop))))
	      (by-buff   (aten/engine:uop->buffer (car (last by-uop))))
	      (range    (spawn-range bind from-buff to-buff by-buff)))
	 (setf (gethash bind scope) range)
	 (prog1
	     `(,@from-uop
	       ,@to-uop
	       ,@by-uop
	       ,(aten/engine:make-uop-loop
		 :iters (getscope bind)
		 :scope :global)
	       ,@(apply #'append (map 'list #'explore (cddr form)))
	       ,(aten/engine:make-uop-endloop
		 :iters (getscope bind)
		 :option :none))
	   (remhash bind scope))))
      ;; (- a) -> -a
      ((list '- form)
       (error "not ready")
       )
      
      ;; arithmetic
      ((list* (or '+ '- '* '/ '> '>= '< '<= '=) _)
       (let ((car  (car form))
	     (args (map 'list #'explore (cdr form))))
	 `(,@(apply #'append args)
	   ,(aten/engine:make-uop-alu
	     :x-writes
	     (prog1
		 (list (read-counter counter 'alu))
	       (incf (counter-alu counter)))
	     :x-reads
	     (loop for arg in args
		   append (list (aten/engine:uop->buffer (car (last arg)))))
	     :op-type (intern (symbol-name car) "KEYWORD")))))

      ;; A = B, A+=B, A-=B, A*=B, A/=B
      ((list (or 'incf 'decf 'mulcf 'divcf) form1)
       (error "not ready"))
      
      ((list (or 'incf 'decf 'mulcf 'divcf) to what)
       (let* ((op (case (car form)
		    (incf '+)
		    (decf '-)
		    (mulcf '*)
		    (divcf '/)))
	      (to-aref-p (and (listp to) (eql (car to) 'aref)))
	      (to-buffer (if to-aref-p
			     `(aref ,@(cdr to))
			     to)))
	 
	 (explore
	  `(setf
	    ,to-buffer
	    (,op
	     ,to-buffer
	     ,what)))))

      ((list 'setf to what)
       (let ((to   (explore to))
	     (what (explore what)))
	 (assert (aten/engine::uop-load-p (car to)) () "Assertion failed: X = Y; X must be a LOAD.")
	 `(,@what
	   ,(aten/engine:make-uop-store
	     :x1 (aten/engine::uop-load-x2 (car to))
	     :x2 (aten/engine:uop->buffer (car (last what)))))))

      ;; (if exp then &optional else)

      ((list* 'if cond _)
       (error "not ready")
       )

      ;; progn -> { forms }

      ((list* 'progn _)
       `(,@(apply #'append (map 'list #'explore (cdr form)))))

      ;; aref -> A[idx]
      ;; Return: Buffer
      ((list* 'aref (type symbol) _)
       (let* ((subscripts (map 'list #'explore (cddr form)))
	      (buffer (aten/engine:make-aref-buffer
		       :idx  (map 'list #'(lambda (x) (aten/engine:uop->buffer (car (last x)))) subscripts)
		       :name (getscope (second form)))))
	 (append
	  (alexandria:flatten subscripts)
	  (list
	   (aten/engine:make-uop-load
	    :x1 (prog1
		    (read-counter counter 'val)
		  (incf (counter-val counter)))
	    :x2 buffer)))))

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
       (let ((val (aten/engine:make-const-buffer :value (getscope form))))
	 (list
	  (aten/engine:make-uop-load
	   :x1 (prog1
		   (read-counter counter 'val)
		 (incf (counter-val counter)))
	   :x2 val))))
      
      ;; string
      ;;((type string) (format nil "\"~a\"" form))
      (_
       (error "trace-uops: Cannot deal with this form: ~a" form)))))

(defun trace-uops (inputs body &aux (scope (make-hash-table)) (counter (make-counter)))
  "Creates a computation graph from lisp-like DSL"
  (declare (type list inputs body))

  (dolist (input inputs)
    (setf (gethash (intern (symbol-name (aten/ir:aten-id input))) scope) input)
    ;; Register symbols
    (dolist (shape (aten/ir:aten-shape input))
      (setf (gethash (intern (symbol-name shape)) scope) shape)))
  
  (graph-funcall counter scope body))

