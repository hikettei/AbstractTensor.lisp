
;;
;; This file provides hard-coded optimization techniques dedicated to deep learning.
;;

(in-package :abstracttensor/engine)

(defun explore-uopgraph (uops value &key (finder #'uop-writes))
  (declare (type list uops))
  (when (or (symbolp value) (stringp value))
    (let ((results))
      (loop for u in uops do
	(let ((users (funcall finder u)))
	  (typecase users
	    (string
	     (when (equal users value)
	       (push u results)))
	    (list
	     (dolist (u1 users)
	       (when (and (stringp u1) (equal u1 value))
		 (push u results))))
	    (T
	     (when (typep users 'uop-load)
	       (let ((x1 (funcall finder (uop-load-x2 users))))
		 (dolist (xn x1)
		   (when (equal xn value)
		     (push u results)))))

	     (when (typep users 'uop-loop)
	       (let ((x1 (funcall finder users)))
		 (dolist (xn x1)
		   (when (equal xn value)
		     (push u results)))))))))
      (remove-duplicates (reverse results)))))

(defun value->users (graph value)
  (declare (type UOpGraph graph))
  (explore-uopgraph (uopgraph-uops graph) value :finder #'uop-reads))

(defun uops/value->users (uops value)
  (explore-uopgraph uops value :finder #'uop-reads))

(defun user->values (graph value)
  (declare (type UOpGraph graph))
  (explore-uopgraph (uopgraph-uops graph) value :finder #'uop-writes))

(defun uops/user->values (uops value)
  (explore-uopgraph uops value :finder #'uop-writes))

(defun recursively-find-parents (graph value)
  (declare (type UOpGraph graph))
  
  (let ((keys (typecase value
		(string (list value))
		(symbol (list value))
		(T      value))))

    (when (not (listp keys))
      (setf keys (list keys)))
    
    (loop for k in keys
	  if (or (symbolp k) (stringp k))
	    append
	    (let ((results (user->values graph k)))
	      (if results
		  (alexandria:flatten
		   (append
		    results
		    (map
		     'list
		     #'(lambda (x)
			 (when (not (equal x value))
			   (recursively-find-parents graph x)))
		     results)))
		  (list k)))
	  else
	    append (recursively-find-parents graph (uop-reads k)))))

(defun recursively-find-deps (uops value)
  "Recursively explores what ids the value depends on."
  (declare (type list uops))

  (when (not (typep value 'graph-id))
    (return-from recursively-find-deps nil))
  
  (let* ((readers (uops/user->values uops value))
	 (buffers (map 'list #'uop->buffer readers)))
    (append
     buffers
     (apply
      #'append
      (map
       'list
       #'(lambda (x)
	   (when (not (equal x value))
	     (recursively-find-deps uops x)))
       buffers)))))

;; deprecated?
(defun compute-determined-iters (uops)
  (let ((iters))
    (dolist (uop uops)
      (when (uop-loop-p uop)
	(setf iters (append iters (uop-writes uop))))
      (when (uop-endloop-p uop)
	(let ((rms (range-id (uop-endloop-iters uop))))
	  (dolist (r rms)
	    (setf iters (remove r iters))))))
    iters))

(defun %uops-fix-loop-scope (graph)
  "Note that this function destructively modifies the graph. Push uops upward out of loop if it does not depend on the loop"
  (declare (type UOpGraph graph))
  (let ((loop-stacks))
    (loop for u in (uopgraph-uops graph) do
      (cond
	((null (car (last loop-stacks)))
	 (setf loop-stacks `(,@loop-stacks ,u)))
	((typep u 'UOp-Loop)
	 (setf loop-stacks `(,@loop-stacks (,u))))
	((not (find (type-of u) '(UOp-Const UOp-ALU UOp-Cast UOp-Load)))
	 (setf loop-stacks `(,@(butlast loop-stacks)
			     (,@(last loop-stacks) ,u))))
	(T
	 ;; U is asserted that one of Const, ALU, Cast, Load
	 (let ((parents (recursively-find-parents graph u)))
	   ;; dont push if any local buffers because there might have STORE and BARRIER (not considered as parent) between DEFINE_LOCAL and here
	   (if (some #'uop-define-local-p parents)
	       (setf loop-stacks `(,@(butlast loop-stacks)
				   (,@(last loop-stacks) ,u)))
	       ;; check backwards and put the uop in the first encounter with some dependency
	       ;; parents is sorted as: [younger, ... , older]
	       (loop named dep-check
		     for idx downfrom (1- (length loop-stacks)) to 0
		     for itrs = (compute-determined-iters (alexandria:flatten (list (nth idx loop-stacks))))
		     if (or
			 (= idx 0)
			 (some
			  #'(lambda (x) (find x parents :test #'equal))
			  `(,@itrs ,@(nth idx loop-stacks))))
		       do (progn
			    (setf (nth idx loop-stacks) (alexandria:flatten (append (list (nth idx loop-stacks)) (list u))))
			    (return-from dep-check))))))))
    (setf (uopgraph-uops graph) (alexandria:flatten loop-stacks))))

(defun %uops-simplify-loops (graph)
  (declare (type UOpGraph graph))

  )

(defun %uops-optimize-loops (graph)
  (declare (type UOpgraph graph))

  ;; Fix loop scope, push uops upward out of loop if it does not depend on the loop
  (%uops-fix-loop-scope graph)

  ;; Simplifies the loop
  (%uops-simplify-loops graph)

  ;; fix-to-store-directly (vectorize)
  )


(defun %uops-insert-stride-computation (graph)
  (declare (type UOpGraph graph))

  ;; It creates a Simplifier scope that is independent of the external environment.
  ;; (define-simplifiers here will produce no sideffects outside of this function.)

  )

(defun uops-optimize (uops)
  "## [function] uop-optimize

```
(uops-optimize uops)
```

This is the top-level function for compiling UOps. Based on the compilation details specified in runtime, it rewrites the hardware-independent UOps into hardware-dependent graphs, which are then translated into each respective language.

### Usage

```lisp
(with-clang-runtime
   (uops-optimize uops))
```
"
  (declare (type list uops))
  
  (assert *runtime* () "uops-optimize: *runtime* is not declared.")

  ;; Here, we are going to call a set of optimization techniques. Function defined with % is destructive.
  ;; A lot of optimization stuffs are behind
  ;; - ./uops-simplifier.lisp (DAG Fusion, Constant Folding, etc)
  ;; - ./uops-optimizer.lisp  (Unsafe Optimizations, Loop Optimization Techniques, etc)
  
  (let* ((graph  (make-uopgraph uops)))
    ;; If :flatten is specified for indexing-rule, it inserts the calculation of strides.

    (when (equal (runtimeconfig-indexing-rule *runtime*) :flatten)
      (%uops-insert-stride-computation graph))

    
    ;; [TODO] SIMD/Shader用途にLoopを最適化
    
    ;; [./uops-simplifier.lisp]
    ;; Simplifies the DAG Graph.
    ;; e.g.: Constant Propagate, removes an isolated graph, op fusion if possible.
    (%uopgraph-simplify           graph)
    
    ;; Update the saved-expr table.
    ;;(%uopgraph-update-saved-exprs graph)

    ;; (maphash
    ;;  #'(lambda (k v)
    ;;	  (format t "~a -> ~a~%" k v))
    ;;    (uopgraph-saved-exprs graph))

    ;; [./uops-optimizer.lisp]
    ;; Applies loop-oriented optimization techniques
    (%uops-optimize-loops graph)

    ;; Finally, Simplifies the DAG Graph Again.
    (%uopgraph-simplify graph)
    graph
    ))

