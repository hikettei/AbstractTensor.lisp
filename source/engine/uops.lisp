
;;
;; uops.lisp
;;
;; Codes are heavily inspired from tinygrad. https://github.com/tinygrad/tinygrad/blob/master/tinygrad/codegen/uops.py
;; ** Use C-x C-k to debug this file on REPL! **
;; 結局解決できてないこと
;; - 1. ShapeがDynamicのままで最適化できるのか？
;; - 2. Broadcast Notation ~をどう実装するか？

(in-package :abstracttensor/engine)

;; ~~ Range Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(deftype graph-id ()
  `(or symbol string))

(defstruct Range
  "Range: for (int id=from; from<to; id+=by)"
  (id)
  (from)
  (to)
  (by))

(defmethod print-object ((obj range) stream)
  (format stream "<Range[~a]: from ~a to ~a by ~a>"
	  (range-id obj)
	  (range-from obj)
	  (range-to obj)
	  (range-by obj)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern (with-output-to-string (c) (dolist (a args) (princ a c))))))

;; ~~ UOPs Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Read/Write Dependencies
(defgeneric uop-writes (uop)
  (:documentation
   "This generic method refers the write dependency of each UOPs."))

(defgeneric uop-reads  (uop)
  (:documentation
   "This generic method refers the read dependency of each UOPs."))

;; When encountered Special UOps (e.g.: Loop), uop-reads/writes should return nil to keep consistency.
(defmethod uop-writes ((uop t)) nil)
(defmethod uop-reads ((uop t)) nil)
(defun eliminate-buffer (list f)
  ;; Recursively explores until buffer elimiated
  (if (every #'(lambda (x) (typep x 'graph-id)) list)
      list
      (alexandria:flatten
       (map 'list #'(lambda (x) (if (typep x 'graph-id) x (funcall f x))) list))))
	   
(defmethod uop-writes :around (uop)
  (let ((result (call-next-method)))
    (eliminate-buffer
     (if (listp result)
	 result
	 (list result))
     #'uop-writes)))

(defmethod uop-reads :around (uop)
  (let ((result (call-next-method)))
    (eliminate-buffer
     (if (listp result)
	 result
	 (list result))
     #'uop-reads)))


;; Each time C-c C-x this eval-when form, the macro uopcase is redefined.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *uop-features* (make-hash-table)    "HashTable: UOP-NAME  -> (Slots)")
  (defparameter *buffer-features* (make-hash-table) "HashTable: BUFF-NAME -> (Slots)")

  (defmacro define-uop (name docs slots &key (write) (read))
    `(progn
       (export ',(symb 'make- 'uop- name))
       (defstruct ,(symb 'uop- name)
	 ,docs
	 ,@slots)

       (setf (gethash ',name *uop-features*) ',(map 'list #'car slots))
       (defmethod uop-writes ((uop ,(symb 'uop- name))) ,write)
       (defmethod uop-reads ((uop ,(symb 'uop- name)))  ,read)))

  (defmacro define-buffer (name docs slots &key (write) (read))
    `(progn
       (export ',(symb 'make- name '-buffer))
       (defstruct ,(symb name '-buffer)
	 ,docs
	 ,@slots)

       (setf (gethash ',name *buffer-features*) ',(map 'list #'car slots))
       (defmethod uop-writes ((buffer ,(symb name '-buffer))) ,write)
       (defmethod uop-reads  ((buffer ,(symb name '-buffer))) ,read)))

  ;; ~~ Buffers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  (define-buffer Const
    "Loads a constant scalar value."
    ((value nil)
     (type :float :type keyword)
     (pointer-p nil :type boolean)))

  (define-buffer Aref
    "Refs [idx] from [name]"
    ((name nil)
     (idx nil))
    :read (aref-buffer-idx buffer))

  (deftype Buffers ()
    `(or Const-Buffer Aref-Buffer string))

  ;; ~~ UOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (define-uop Loop
    "
## Uop[Loop]
```
Loop iter, scope.
```
"
    ((iters nil :type Range)
     (scope :global :type (and keyword (member :global :local))))
    ;; [TODO] IteratorがReadsで使ってる変数に依存がある！！！
    ;; e.g.: range from 0 to a
    ;; 今は全部整数を仮定してるけど，aとかをreadsにする
    :write (list (range-id (uop-loop-iters uop))))

  (define-uop EndLoop
    "
## Uop[EndLoop]
```
EndLoop [iters0 iters1 iters2 ...], option
```
As of now, option is one of following:
- :reduce
- :none
"
    ((iters nil :type Range)
     (option :none :type (and keyword (member :none :reduce)))))

  (define-uop If
    "
## Uop[If]
```
If [Condition] [ID]
```
"
    ((condition nil)
     (id nil)))

  (define-uop EndIf
    "
## Uop[EndIF]
```
EndIF [ID]
```
"
    ((id nil)))

  (define-uop Special
    ""
    ())

  (define-uop define-global
    ""
    ())

  (define-uop define-var
    ""
    ())

  (define-uop define-local
    ""
    ())

  (define-uop define-acc
    ""
    ())

  (define-uop Load
    "
## UOp[Load]
```
Load [x1] [x2]
```
Stores x2 into x1.
"
    ((x1 nil :type Buffers)
     (x2 nil :type Buffers))
    :read  (uop-load-x2 uop)
    :write (uop-load-x1 uop))

  (define-uop Store
    "
## UOp[Store]
```
Store [x1] [x2]
```
Stores the value of buffer x2 into x1.
"
    ((x1 nil :type Buffers)
     (x2 nil :type string))
    :read  (uop-store-x2 uop)
    :write (uop-store-x1 uop))

  (define-uop Const
    ""
    ())

  (define-uop barrier
    ""
    ())

  (define-uop PHI
    ""
    ())

  (define-uop ALU
    "
## UOp[ALU]
```
ALU [x_writes1 x_writes2] [x_read1 x_read2 ...], op-type
```
"
    ((x-writes nil :type list)
     (x-reads nil :type list)
     (op-type nil))
    :read  (uop-alu-x-reads uop)
    :write (uop-alu-x-writes uop))

  (define-uop WMMA
    ""
    ())

  (define-uop Cast
    ""
    ())

  (define-uop Bitcast
    ""
    ())

  (define-uop Gep
    ""
    ())

  (define-uop noop
    ""
    ())
  ) ;; eval-when


(macrolet ((with-package (&body body) `(let ((*package* (find-package :abstracttensor/engine))) ,@body)))
  (flet ((uop-typep (name)   (with-package (symb 'uop- name '-p)))
	 (buffer-typep (name) (with-package (symb name '-buffer-p))))
    #.`(defmacro uopcase
	   (keyform
	    &key
	      ,@(loop for uop-name being each hash-key of *uop-features*
			using (hash-value slots)
		      collect
		      `(,uop-name '((,@slots) (warn "[uopcase]: UOp ~a fell through." ',uop-name)))))
	 "TODO: Docs"
	 `(cond
	    ,,@(loop for uop-name being each hash-key of *uop-features*
		       using (hash-value slots)
		     collect
		     `(progn
			(when (not (= (length ',slots) (length (car ,uop-name))))
			  (warn "[macroexpand] uopcase: the slots and binds for UOp-~a seems incorrect.~%  Expected: ~a~%  Butgot: ~a~%" ',uop-name ',slots (car ,uop-name)))
			`((,(uop-typep ',uop-name) ,keyform)
			  (let (,@(map
				    'list
				    #'(lambda (bind slot-name)
					`(,bind (slot-value ,keyform ',slot-name)))
				    (car ,uop-name) ',slots))
			    ;;(declare (ignorable ,@',slots))
			    ,@(cdr ,uop-name)))))
	    (T (error "~a is not a family of UOps." ,keyform))))

    #.`(defmacro buffercase
	   (keyform
	    &key
	      ,@(loop for buffer-name being each hash-key of *buffer-features*
			using (hash-value slots)
		      collect
		      `(,buffer-name '((,@slots) (warn "[buffercase]: Buffer ~a fell through." ',buffer-name)))))
	 "TODO: Docs"
	 `(cond
	    ,,@(loop for buffer-name being each hash-key of *buffer-features*
		       using (hash-value slots)
		     collect
		     `(progn
			(when (not (= (length ',slots) (length (car ,buffer-name))))
			  (warn "[macroexpand] buffercase: the slots and binds for Buffer-~a seems incorrect.~%  Expected: ~a~%  Butgot: ~a~%" ',buffer-name ',slots (car ,buffer-name)))
			`((,(buffer-typep ',buffer-name) ,keyform)
			  (let (,@(map
				    'list
				    #'(lambda (bind slot-name)
					`(,bind (slot-value ,keyform ',slot-name)))
				    (car ,buffer-name) ',slots))
			    ;;(declare (ignorable ,@',slots))
			    ,@(cdr ,buffer-name)))))
	    (T (error "~a is not a family of Buffers." ,keyform))))))

;; ~~ UOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(defun uop->buffer (uop)
  "Receives a UOp and identifies which buffer contains the value.
write <- read
^ this function finds out \"write\" of each uops."
  (or
   (let ((out (uop-writes uop)))
     (if (listp out)
	 (progn
	   ;; Note: this assertion could be unnecessary;
	   ;; note that just the returned value of ALU.writes is multiple.
	   ;; (assert (= (length out) 1))
	   (car out))
	 out))
   ;;(error "~a cannot be a buffer." uop)
   uop))

(defstruct (UOpGraph
	    (:constructor make-uopgraph (uops)))
  (uops uops :type list)

  ;; A pair of: val_XXX and UOP
  (saved-exprs (make-hash-table) :type hash-table))

(defun %uopgraph-update-saved-exprs (graph)
  (declare (type UOpGraph graph))
  (let ((new-table (make-hash-table :test 'equal)))
    (loop for u in (uopgraph-uops graph) do
      (let ((w (uop-writes u)))
	;; Collecting a pair of alu and Ops of:
	;; val_x = xxx
	;; alu_x = xxx
	(when (stringp w)
	  (setf (gethash w new-table) u))
	(when (listp w)
	  (dolist (wn w)
	    (when (stringp wn)
	      (setf (gethash wn new-table) u))))))
    (setf (uopgraph-saved-exprs graph) new-table)))		   

;;(defun uop-vars (graph)
;;  (declare (type UOpGraph graph))
;;  (loop for op in (uopgraph-uops graph)
;;	if (uop-define-global-p op)
;;	  collect op))

(defun uops-optimize (uops)
  "## [function] uop-optimize
Returns a list of optimized uops graph
"
  (declare (type list uops))

  ;; Tinygrad: toplevel koko https://github.com/tinygrad/tinygrad/blob/master/tinygrad/codegen/linearizer.py#L322

  ;; Here, we are going to call a set of optimization techniques. Function defined with % is destructive.
  ;; A lot of optimization stuffs are behind
  ;; - ./uops-simplifier.lisp (DAG Fusion, Constant Folding, etc)
  ;; - ./uops-optimizer.lisp  (Unsafe Optimizations, Loop Optimization Techniques, etc)
  (let* ((graph  (make-uopgraph uops)))
    ;; SIMD/Shader用途にLoopを最適化

    
    ;; [./uops-simplifier.lisp]
    ;; Simplifies the DAG Graph.
    (%uopgraph-simplify           graph)
    ;; Update the saved-expr table.
    (%uopgraph-update-saved-exprs graph)

    ;;(maphash
    ;; #'(lambda (k v)
    ;;	 (format t "~a -> ~a~%" k v))
    ;;   (uopgraph-saved-exprs graph))

    ;; [./uops-optimizer.lisp]
    ;; Applies loop-oriented optimization techniques
    (%uops-optimize-loops graph) 
    graph
    ))

