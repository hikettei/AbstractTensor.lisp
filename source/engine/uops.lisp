
;;
;; uops.lisp
;;
;; Codes are heavily inspired from tinygrad. https://github.com/tinygrad/tinygrad/blob/master/tinygrad/codegen/uops.py
;; ** Use C-x C-k to debug this file on REPL! **

(in-package :abstracttensor/engine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest args)
    (intern (with-output-to-string (c) (dolist (a args) (princ a c))))))

;; ~~ UOPs Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; The lowest level IR, UOP
;; UOP is defined as: op-name (read-args) (write-args), [options]
;; (TODO: Description)

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
  (let ((result
	  (alexandria:flatten
	   (map
	    'list
	    #'(lambda (x)
		(if (and (typep x 'buffers) (not (typep x 'graph-id)))
		    (funcall f x)
		    x))
	    list))))
    (loop for r in result
	  if (typep r 'graph-id) collect r)))

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
    "Loads a constant scalar value.
Const could be one of: number string aten/ir:AbstractTensor[Scalar] keyword symbol"
    ((value nil   :type (or number string aten/ir:AbstractTensor))
     (type :float :type Dtypes)
     (pointer-p nil :type boolean))
    :read (typecase (const-buffer-value buffer)
	    (aten/ir:AbstractTensor (list (aten/ir:aten-id (const-buffer-value buffer))))
	    (T (list (const-buffer-value buffer))))
    :write nil)

  (define-buffer Aref
    "Refs [idx] from [name]."
    ((name    nil :type aten/ir:AbstractTensor)
     (idx     nil :type list))
    :read (append
	   (loop for x in (aref-buffer-idx buffer)
		 if (not (numberp x)) collect x)
	   (list (aten/ir:aten-id (aref-buffer-name buffer)))))

  (define-buffer Packed
    "Packed [packed-object1 packed-object2 packed-pbject3 ...]
Refs can be nil or a list of numbers.
If specified, render the code like:
    (const dtype[]){__object_}"
    ((packed-objects nil :type (and list (satisfies buffer-list-p)))
     (dtype nil :type Dtypes))
    :read  (loop for x in (packed-buffer-packed-objects buffer)
		 if (stringp x)
		   append (list x)
		 else
		   append (uop-reads x))
    :write  (loop for x in (packed-buffer-packed-objects buffer)
		  if (stringp x)
		    append (list x)
		  else
		    append (uop-writes x)))
  
  (deftype Buffers ()
    `(or Const-Buffer Aref-Buffer Packed-Buffer String))
  
  (defun buffer-list-p (x) (every #'(lambda (x) (typep x 'Buffers)) x))

  ;; ~~ Range Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  (deftype graph-id ()
    `(or string))

  (defstruct Range
    "Range: for (int id=from; from<to; id+=by)"
    (id ""   :type graph-id)
    (from "" :type (or number Buffers))
    (to ""   :type (or number Buffers))
    (by ""   :type (or number Buffers)))

  (defmethod print-object ((obj range) stream)
    (format stream "<Range[~a]: from ~a to ~a by ~a>"
	    (range-id obj)
	    (range-from obj)
	    (range-to obj)
	    (range-by obj)))

  ;; ~~ UOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (define-uop Defun
    "
## UOp[Defun]

"
    ((inputs nil :type list)
     (outputs nil :type list)
     (named  "" :type string)))

  (define-uop EndDefun
    "
## UOp[EndDefun]

"
    ((named "" :type string)))
  
  (define-uop Loop
    "
## Uop[Loop]
```
Loop iter, scope.
```
"
    ((iters nil :type Range))

    :write (list (range-id (uop-loop-iters uop)))
    :read  (let ((range (uop-loop-iters uop)))
	     (list
	      (when (not (numberp (range-from range)))
		(range-from range))
	      (when (not (numberp (range-to range)))
		(range-to range))
	      (when (not (numberp (range-by range)))
		(range-by range)))))

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
    ((iters nil :type Range))
    :write nil
    :read  (let ((range (uop-endloop-iters uop)))
	     (list
	      (when (not (numberp (range-from range)))
		(range-from range))
	      (when (not (numberp (range-to range)))
		(range-to range))
	      (when (not (numberp (range-by range)))
		(range-by range)))))

  (define-uop If
    "
## Uop[If]
```
If [Condition]
```
"
    ((condition nil :type Buffers)))

  (define-uop EndIf
    "
## Uop[EndIF]
```
EndIF
```
"
    ())

  (define-uop Special
    ""
    ())

  (define-uop define-global
    ""
    ())

  (define-uop define-var
    ""
    ())

  (define-uop declare-var
    "
## UOp[declare-var]
Ignore this op when compiling.
"
    ((var nil :type graph-id)
     (dtype :float :type dtypes)
     (pointer-p nil :type boolean))
    :write (list (uop-declare-var-var uop)))

  (define-uop define-local
    "
## UOp[define-local]
Ignore this op when compiling."
    ((var nil :type graph-id))
    :write (list (uop-define-local-var uop)))

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
    ((x1 nil :type String) ;; x1=string?
     (x2 nil :type Buffers)
     (reduction nil :type (or null string)))
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
     (x2 nil :type String)
     (reduction nil :type (or null string)))
    :read  (append
	    (list (uop-store-x2 uop))
	    ;; Aref[X1, X2] <- ZZZ
	    ;;     ^ Store depends on X1 and X2
	    (when (aref-buffer-p (uop-store-x1 uop))
	      (aref-buffer-idx (uop-store-x1 uop))))
    
    :write (uop-store-x1 uop))

  ;; Not anymore used?
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
     (op-type nil :type Operators)
     (dtype   nil :type Dtypes)
     (reduction nil :type (or null string)))
    :read  (apply #'append (map 'list #'uop-reads (uop-alu-x-reads uop)) (list (uop-alu-x-reads uop)))
    :write (uop-alu-x-writes uop))
  (defmethod print-object ((alu UOp-ALU) stream)
    (format stream "{ALU [~a][~a]: ~a -> ~a}~%"
	    (uop-alu-op-type alu)
	    (uop-alu-dtype alu)
	    (uop-alu-x-reads alu)
	    (uop-alu-x-writes alu)))

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
			    (declare (ignorable ,@(map 'list #'(lambda (x) x) (car ,uop-name))))
			    ,@(cdr ,uop-name)))))
	    (T (error "~a is not a family of UOps." ,keyform))))

    #.`(defmacro buffercase
	   (keyform
	    &key
	      (string `((value) (warn "[buffercase] string fell through. ~a" value)))
	      ,@(loop for buffer-name being each hash-key of *buffer-features*
			using (hash-value slots)
		      collect
		      `(,buffer-name '((,@slots) (warn "[buffercase]: Buffer ~a fell through." ',buffer-name)))))
	 "TODO: Docs"
	 `(cond
	    ((stringp ,keyform)
	     (funcall #'(lambda ,@string) ,keyform))
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
			    (declare (ignorable ,@(map 'list #'(lambda (x) x) (car ,buffer-name))))
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
	 (eliminate-buffer out #'uop-writes)))
   (when (typep uop 'aref-buffer) uop)
   ;;(error "~a cannot be a buffer." uop)
   nil))

(defstruct (UOpGraph
	    (:constructor make-uopgraph (uops)))
  (uops uops :type list))

