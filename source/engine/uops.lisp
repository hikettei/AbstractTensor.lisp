
;;
;; uops.lisp
;;
;; Codes are heavily inspired from tinygrad. https://github.com/tinygrad/tinygrad/blob/master/tinygrad/codegen/uops.py
;;
;; 結局解決できてないこと
;; - 1. ShapeがDynamicのままで最適化できるのか？
;; - 2. Broadcast Notation ~をどう実装するか？

(in-package :abstracttensor/engine)

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

(defmacro define-uop (name docs slots)
  `(progn
     (export ',(symb 'make- 'uop- name))
     (defstruct ,(symb 'uop- name)
       ,docs
       ,@slots)))

(defmacro define-buffer (name docs slots)
  `(progn
     (export ',(symb 'make- name '-buffer))
     (defstruct ,(symb name '-buffer)
       ,docs
       ,@slots)))

;; ~~ Buffers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(define-buffer Const
  "Loads a constant scalar value."
  ((value nil)))

(define-buffer Aref
  "Refs [idx] from [name]"
  ((name nil)
   (idx nil)))

(deftype Buffers ()
  `(or Const-Buffer Aref-Buffer string))

;; ~~ UOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-uop Loop
  "
## Uop[Loop]
```
Loop [iters0 iters1 iters2 ...], scope.
```

(e.g.: translated into:)
```
for (iters0=0...X) {
    for (iters1=0...Y) {
        for(iters2=0..Z) {
            ....
```
"
  ((iters nil :type list)
   (scope :global :type (and keyword (member :global :local)))))

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
  ((iters nil :type list)
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
   (x2 nil :type Buffers)))

(define-uop Store
  "
## UOp[Store]
```
Store [x1] [x2]
```
Stores the value of buffer x2 into x1.
"
  ((x1 nil :type UOp-Load)
   (x2 nil :type string)))

(define-uop const
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
   (op-type nil)))

(define-uop WMMA
  ""
  ())

(define-uop Cast
  ""
  ())
(define-uop bitcast
  ""
  ())
(define-uop gep
  ""
  ())
(define-uop noop
  ""
  ())

(defun uop->buffer (uop)
  "Receives a UOp and identifies which buffer contains the value."
  (typecase uop
    (UOp-Load (uop-load-x1 uop))
    (UOp-ALU  (car (uop-alu-x-writes uop)))
    (Buffers uop)
    (T (error "~a cannot be a buffer." uop))))

(defstruct (UOpGraph
	    (:constructor make-uopgraph (uops)))
  "A list of UOps"
  (uops uops :type list)
  (saved-exprs (make-hash-table) :type hash-table))

(defun uop-vars (graph)
  (declare (type UOpGraph graph))
  (loop for op in (uopgraph-uops graph)
	if (uop-define-global-p op)
	  collect op))

(defun uop-optimize (uops)
  "## [function] uop-optimize
Returns a list of optimized uops graph
"
  (declare (type list uops))

  ;; A lot of optimization stuffs are behind
  ;; - ./uops-simplifier.lisp (DAG Fusion, Constant Folding, etc)
  ;; - ./uops-optimizer.lisp  (Unsafe Optimizations, Loop Optimization Techniques, etc)
  (let* ((uops   (uops-simplify uops))
	 (graph  (make-uopgraph uops))
	 ;; First, optimizes the iterations
	 (graph1 (uops-optimize-loops graph)))

    graph1
    ))
