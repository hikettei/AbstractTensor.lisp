
(in-package :abstracttensor/lang)

(defstruct Counter
  "gensym alternative"
  (cache (make-hash-table :test #'equalp) :type hash-table)
  (val 0 :type fixnum)
  (alu 0 :type fixnum))

(defun read-counter (counter name)
  (declare (type Counter counter)
	   (type symbol name))
  
  (format nil "_~(~a~)_~a" name (slot-value counter name)))

(defun spawn-range (id from to by)
  (flet ((->gid (value)
	   (if (symbolp value)
	       (symbol-name value)
	       value)))
    (make-range :id (->gid id) :from (->gid from) :to (->gid to) :by (->gid by))))

(defun lazy-stride-statement (subscripts permute shape)
  "Compute the strides for a given shape and a permutation of dimensions."
  (let* ((n (length shape))
         (strides (make-list n)))
    ;; Initialize the stride of the last dimension in permuted order to 1
    (setf (nth (first (reverse permute)) strides) (list 1))
    ;; Calculate strides for other dimensions in reverse permuted order
    (do ((i (- n 2) (- i 1))) ((< i 0) nil)
      (setf (nth (nth i permute) strides)
            `(,@(nth (nth (1+ i) permute) strides)
              ,(nth (nth (1+ i) permute) shape))))
    ;; Reorder the strides to match the original dimension indices
    (let ((strides (mapcar (lambda (i) (nth i strides)) (loop for i from 0 below n collect i))))
      ;; Compute the index
      `(+
	,@(loop for ref in subscripts
		for stride in strides
		if (equal stride `(1))
		  collect ref
		else
		  collect `(* ,ref ,@stride))))))

;; TODO: Replace it with Elegant BNF Parser
;; Feature Enhancement
;; (op X{Array} Y{Array})
;; -> (dotimes (x ...)
;; ->   (dotimes (y ...)
;; ->       (op ... mitaini suru)))
;; - TODO: Imrpove the quality of error message
;; - TODO: this tracer is experimental. refactor this file for furure release.
(defun graph-funcall (counter scope form)
  (flet ((explore (code)
	   (graph-funcall counter scope code))
	 (getscope (name &aux (name (if (symbolp name) (symbol-name name) name)))
	   (or (gethash name scope)
	       (error "~a is not defined." name)))
	 (cache (value id dtype)
	   (or
	    (gethash value (counter-cache counter))
	    (setf (gethash value (counter-cache counter)) (cons id dtype))))
	 (read-cache (content)
	   ;; Equivalent to doing tpsort
	   (car (gethash content (counter-cache counter)))))
	    
    (trivia:ematch form
      ;; Iteration over var
      ;; (dotimes (i var) ..)
      ((list* 'dotimes (list bind count) _)

       ;; binds the iterator variables into bind
       (let* ((count (explore count))
	      (count-idx (aten/engine:uop->buffer (car (last count))))
	      (bind (symbol-name bind))
	      (range (spawn-range bind 0 count-idx 1)))
	 (setf (gethash bind scope) (cons bind :int))
	 (prog1
	     `(,@count
	       ,(aten/engine:make-uop-loop
		 :iters range)
	       ,@(apply #'append (map 'list #'explore (cddr form)))
	       ,(aten/engine:make-uop-endloop
		 :iters range))
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
	      (bind (symbol-name bind))
	      (from-buff (aten/engine:uop->buffer (car (last from-uop))))
	      (to-buff   (aten/engine:uop->buffer (car (last to-uop))))
	      (by-buff   (aten/engine:uop->buffer (car (last by-uop))))
	      (range     (spawn-range bind from-buff to-buff by-buff)))
	 (setf (gethash bind scope) (cons bind :int))
	 (prog1
	     `(,@from-uop
	       ,@to-uop
	       ,@by-uop
	       ,(aten/engine:make-uop-loop
		 :iters range)
	       ,@(apply #'append (map 'list #'explore (cddr form)))
	       ,(aten/engine:make-uop-endloop
		 :iters range))
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
		 (list "non_determined")
		 :x-reads
		 (loop for arg in args
		       append (list (aten/engine:uop->buffer (car (last arg)))))
		 :op-type (intern (symbol-name car) "KEYWORD")
		 ;; Asserting that all dtypes are the same.
		 :dtype dtype))
	      (op1 (aten/engine::copy-uop-alu op))
	      (alu-idx 
		(prog1
		    (list alu)
		  (incf (counter-alu counter)))))
	 (setf
	  (aten/engine::uop-alu-x-writes op)  alu-idx 
	  (aten/engine::uop-alu-x-writes op1) alu-idx)
	 (or (read-cache op)
	     (progn
	       (cache op (list op1) dtype)
	       (setf (gethash alu scope) (cons op dtype))
	       `(,@(apply #'append args) ,op1)))))

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
       (or
	(read-cache form)
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
	  (cache form (list op) dtype)
	  (setf (gethash val-id scope) (cons op dtype))
	  (list
	   op))))
      ;; variable
      ((or (type string) (type symbol))
       (let* ((name (car (getscope form)))
	      (name (or (aten/engine:uop->buffer name) name))
	      (name (if (symbolp name)
			(symbol-name name)
			name))
	      (cache (read-cache name))
	      (_ (if cache (return-from graph-funcall cache)))
	      (val (aten/engine:make-const-buffer :value name :type (cdr (getscope form)) :pointer-p nil))
	      (val-id (read-counter counter 'val))
	      (op  (aten/engine:make-uop-load
		    :x1 (prog1
			    val-id
			  (incf (counter-val counter)))
		    :x2 val)))
	 (declare (ignore _))
	 (cache name (list op) (cdr (getscope form)))
	 (setf (gethash val-id scope) (cons op (aten/engine::const-buffer-type val)))	 
	 (list op)))
      (_
       (error "trace-uops: Cannot deal with this form: ~a" form)))))

(defun tpsort (list &aux (seen))
  (loop for l in list
	for w = (aten/engine::uop-writes l)
	if (and w (find w seen :test #'equalp))
	  do (progn nil)
	else
	  collect (progn (push w seen) l)))

(defun trace-uops (inputs body &aux (scope (make-hash-table :test #'equal)) (counter (make-counter)) (declares))
  "Creates a computation graph from lisp-like DSL.
!! aten/engine::*runtime* must be declared before compilation! to determine stride computation strategy"
  (declare (type list inputs body))
  (assert aten/engine::*runtime* () "trace-uops: *runtime* is not declared.")

  (dolist (input inputs)
    (let ((input-id (symbol-name (aten/ir:aten-id input))))
      (push (aten/engine:make-uop-declare-var :var input-id :dtype (aten/ir:aten-type-class input) :pointer-p (not (null (aten/ir:aten-shape input)))) declares)
      (setf (gethash input-id scope)
	    (cons input (aten/ir:aten-type-class input))))
    
    ;; Register symbols
    (dolist (shape (aten/ir:aten-shape input))
      (when (not (numberp shape))
	(let ((sid (symbol-name shape)))
	  (push (aten/engine:make-uop-declare-var :var sid :dtype :int :pointer-p nil) declares)
	  (setf (gethash sid scope) (cons sid :int))))))

  (tpsort
   `(,@declares
     ,@(graph-funcall counter scope body))))

