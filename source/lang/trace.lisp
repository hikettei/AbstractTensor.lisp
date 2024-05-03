
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

(defun lazy-stride-statement (subscripts strides shape)
  (let ((stacks `(1)))
    `(+
      ,@(loop for s   in strides
	      for sub in subscripts
	      for size = (nth s shape)
	      collect
	      (prog1
		  `(* ,sub ,@stacks)
		(push (if (numberp size)
			  size
			  (intern (symbol-name size)))
		      stacks))))))

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
	 (setf (gethash bind scope) (cons bind :int))
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
	 (setf (gethash bind scope) (cons bind :int))
	 (prog1
	     `(,@from-uop
	       ,@to-uop
	       ,@by-uop
	       ,(aten/engine:make-uop-loop
		 :iters range
		 :scope :global)
	       ,@(apply #'append (map 'list #'explore (cddr form)))
	       ,(aten/engine:make-uop-endloop
		 :iters range
		 :option :none))
	   (remhash bind scope))))
      ;; (- a) -> -a
      ((list '- form)
       (error "not ready")
       )
      
      ;; arithmetic
      ((list* (or '+ '- '* '/ '> '>= '< '<= '=) _)
       (let* ((car  (car form))
	      (args (map 'list #'explore (cdr form)))
	      (alu (read-counter counter 'alu))
	      (dtype
 		(let ((buffer (car (last (car args)))))
		  (typecase buffer
		    (aten/engine::UOp-Load
		     (aten/engine:infer-buffer-type
		      (aten/engine::uop-load-x2
		       buffer)))
		    (aten/engine::UOp-ALU
		     (aten/engine::uop-alu-dtype buffer))
		    (T
		     (error "Cannot infer the type when tracing the graph: ~a" buffer)))))
	      (op
		(aten/engine:make-uop-alu
		 :x-writes
		 (prog1
		     (list alu)
		   (incf (counter-alu counter)))
		 :x-reads
		 (loop for arg in args
		       append (list (aten/engine:uop->buffer (car (last arg)))))
		 :op-type (intern (symbol-name car) "KEYWORD")
		 ;; Asserting that all dtypes are the same.
		 :dtype dtype)))
	 (setf (gethash alu scope) (cons op dtype))
	 `(,@(apply #'append args) ,op)))

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
	   ,@to
	   ,(aten/engine:make-uop-store
	     :x1 (aten/engine::uop-load-x2 (car (last to)))
	     :x2 (aten/engine:uop->buffer  (car (last what)))))))

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
       (let* ((flatten-p (eql (aten/engine::runtimeconfig-indexing-rule aten/engine::*runtime*) :flatten))
	      (subscripts (map 'list #'explore (cddr form)))
	      (aref-name    (car (getscope (second form))))
	      (ref-buffers (map 'list #'(lambda (x) (aten/engine:uop->buffer (car (last x)))) subscripts))
	      (index-uops (if flatten-p
			      (progn
				(assert (aten/ir::abstracttensor-p aref-name) ())
				(let ((strides (aten/ir:aten-order aref-name))
				      (shape   (aten/ir:aten-shape aref-name)))
				  (explore (lazy-stride-statement ref-buffers strides shape))))
			      nil))
	      (index-buffers (if flatten-p
				 (list (aten/engine:uop->buffer (car (last index-uops))))
				 ref-buffers))
	      (buffer (aten/engine:make-aref-buffer
		       :idx  index-buffers
		       :name aref-name))
	      (val (read-counter counter 'val))
	      (op (aten/engine:make-uop-load
		   :x1 (prog1
			   val
			 (incf (counter-val counter)))
		   :x2 buffer)))
	 (setf (gethash val scope) (cons op (aten/ir:aten-type-class aref-name)))
	 (append
	  (alexandria:flatten subscripts)
	  index-uops
	  (list op))))

      ;; funcall. (car cdr)
      ((list* (type symbol) _)
       (error "not ready")
       )
      ;; number
      ((type number)
       (let* ((dtype (typecase form
		       (float :float)
		       (integer :int)
		       (T
			(warn "cannot infer the type of: ~a" form)
			:float)))
	      (val (aten/engine:make-const-buffer :value form :type dtype))
	      (val-id (read-counter counter 'val))
	      (op (aten/engine:make-uop-load
		   :x1 (prog1
			   val-id
			 (incf (counter-val counter)))
		   :x2 val)))
	 (setf (gethash val-id scope) (cons op dtype))
	 (list
	  op)))
      ;; variable
      ((or (type string) (type symbol))
       (let* ((name  (car (getscope form)))
	      (name (if (aten/engine::uop-load-p name)
			(aten/engine::uop-load-x1 name)
			name))
	      (val (aten/engine:make-const-buffer :value name :type (cdr (getscope form)) :pointer-p nil))
	      (val-id (read-counter counter 'val))
	      (op  (aten/engine:make-uop-load
		    :x1 (prog1
			    val-id
			  (incf (counter-val counter)))
		    :x2 val)))
	 (setf (gethash val-id scope) (cons op (aten/engine::const-buffer-type val)))	 
	 (list op)))
      (_
       (error "trace-uops: Cannot deal with this form: ~a" form)))))

(defun trace-uops (inputs body &aux (scope (make-hash-table)) (counter (make-counter)))
  "Creates a computation graph from lisp-like DSL.
!! aten/engine::*runtime* must be declared before compilation! to determine stride computation strategy"
  (declare (type list inputs body))
  (assert aten/engine::*runtime* () "trace-uops: *runtime* is not declared.")

  (dolist (input inputs)
    (setf (gethash (intern (symbol-name (aten/ir:aten-id input))) scope) (cons input (aten/ir:aten-type-class input)))
    ;; Register symbols
    (dolist (shape (aten/ir:aten-shape input))
      (setf (gethash (intern (symbol-name shape)) scope) (cons shape :int))))

  (graph-funcall counter scope body))

