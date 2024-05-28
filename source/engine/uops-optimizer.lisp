
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

(defun recursively-find-deps (uops value)
  "Recursively explores what ids the value depends on."
  (declare (type list uops))

  (when (not (typep value 'graph-id))
    (return-from recursively-find-deps nil))
  
  (let* ((readers (uops/user->values uops value))
	 (read-ids (loop for reader in readers
			 append
			 (loop for r in (uop-reads reader)
			       if (typep r 'graph-id)
				 collect r
			       else
				 collect (uops/user->values uops r))))
	 (buffers (loop for x in (alexandria:flatten read-ids) if x collect x)))
    (remove-duplicates
     (append
      buffers
      (apply
       #'append
       (map
	'list
	#'(lambda (x)
	    (when (not (string= x value))
	      (recursively-find-deps uops x)))
	buffers)))
     :test #'equalp)))

(defun compute-determined-iters (uops)
  (let ((iters))
    (dolist (uop uops)
      (when (uop-loop-p uop)
	(setf iters (append iters (uop-writes uop))))
      (when (uop-endloop-p uop)
	(let ((rms (range-id (uop-endloop-iters uop))))
	  (setf iters (remove rms iters)))))
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
	 (let ((parents (recursively-find-deps (uopgraph-uops graph) (uop->buffer u))))
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

(defun %uops-optimize-loops (graph)
  (declare (type UOpgraph graph))

  ;; Fix loop scope, push uops upward out of loop if it does not depend on the loop
  (%uops-fix-loop-scope graph)

  ;; TODO: Loop Elimination
  ;; e.g.: removes a loop where total_size=1
  )

(defun get-nth-loop (uops nth)
  (loop with c = 0
	for u in uops
	if (uop-loop-p u)
	  do (when (= c nth)
	       (return-from get-nth-loop u))
	     (incf c)))

(defparameter *loop-unrolling-stride* 4) ;; [WIP]
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
  ;; - ./uops-vectorizer.lisp  (for Vector Computor Archs)
  ;; - ./uops-linearlizer.lisp (for Scalar Computor Archs)
  
  ;; 1. Creates the UOpGraph Object.
  (let* ((graph  (make-uopgraph uops)))
    ;; 2. Applies the first (user-defined) simplification process.
    ;; See also: [./uops-simplifier.lisp]
    ;; Applies the simplifiers defined by `define-simplifier` macros.
    ;; e.g.: Constant Propagate, removes an isolated graph, op fusion if possible.
    (%uopgraph-simplify   graph)
    ;; 3. Applies loop-oriented optimization techniques.
    (%uops-optimize-loops graph)
    ;; 4. Before applying linearlizer, we simplifies the UOps graph again for simplicity
    (%uopgraph-simplify graph)
    ;; 5. Applying linearlizer depending on the runtimeconfig.

    ;; 6. Attributing @reduction
    (%uopgraph-optimize-accumlation graph)
    
    ;; 7. Parallelize

    ;; 1. :Unrollをする Failed -> Warning
    ;; 2. :ALU_0_4とかをHashTableにまとめておいて，後でGatherする
    (let ((strategy (runtimeconfig-vectorize-strategy *runtime*)))

      (when (eql strategy :scalar)
	(warn "strategy=:scalar is WIP."))

      ;; strategy is unroll or vector
      ;;  -> First, unroll the loops as many as possible
      (when (or (eql strategy :unroll) (eql strategy :vector))
	(let* ((scope-type (runtimeconfig-scoping-type *runtime*))
	       (simd-table (make-hash-table :test #'equalp))
	       (loops (loop with depth = 0
		            for uop in (uopgraph-uops graph)
			    if (uop-loop-p uop)
			      collect
			      (prog1
				  (cons depth uop)
				(incf depth))
			    if (uop-endloop-p uop)
			      do (decf depth)))
	       (simd-axes (when (eql strategy :vector)
			    (loop for (depth . range) in loops
				  for size = (%uopgraph-blocksize graph (range-id (uop-loop-iters range)))
				  collect
				  (progn
				    (setf (gethash (get-nth-loop uops depth) simd-table) size)
				    size)))))
	  ;; simd-axes ... (loop1 loop2 loop3 ...)
	  ;; the length of simd-axes is not always corresponds with the rank of tensors.
	  (loop for (depth . range) in loops
		for nth upfrom 0
		for blocksize = (if (eql strategy :unroll)
				    *loop-unrolling-stride*
				    (or (nth nth simd-axes) *loop-unrolling-stride*))
		do (%uopgraph-unroll graph (range-id (uop-loop-iters range)) blocksize scope-type))

	  ;; If strategy is :vector, replace unrolled ALU with vectorized ALU.
	  (when (eql strategy :vector)
	    ;; Apply vectorization
	    (%uopgraph-simplify graph)
	    (%vectorize-uops graph simd-table)))))

    (%uopgraph-simplify graph)
    ;; 8. It's all! returns the optimized UOpGraph structure.
    ;; (print graph)
    graph))

