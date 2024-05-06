
;;
;;
;;

(in-package :abstracttensor/engine)


;; Reading list for myself:
;;  - https://github.com/Leslie-Fang/GEMM_Optimization/blob/master/optimization8/optimization8.cpp


;; [TODO] SIMD/Simplifierで後から最適化する。
(defun vectorize-uops (uops unroll-idx unroll-by &aux (seen (list unroll-idx)))
  (labels ((->unroll-idx (name nth)
	     ;; [TODO] delete nth
	     (if (equal (aref name 0) #\_)
		 (format nil "~a_~a" name nth)
		 (format nil "_~a_~a" name nth)))
	   (to-unroll? (uop)
	     (intersection seen (uop-reads uop) :test #'equal))
	   (unroll-buffer? (name)
	     (find name seen :test #'equal))
	   (unroll-buffer (buffer nth)
	     (if (typep buffer 'buffers)
		 (buffercase
		  buffer
		  :packed ((packed-objects)
			   )
		  :string ((value)
			   (if (unroll-buffer? value)
			       (->unroll-idx value nth)
			       value))
		  :const  ((value type pointer-p)
			   (if (unroll-buffer? value)
			       (make-const-buffer :value (->unroll-idx value nth) :type type :pointer-p pointer-p)
			       value))
		  :aref ((name idx)
			 (make-aref-buffer
			  :name name
			  :idx
			  (map
			   'list
			   #'(lambda (x)
			       (if (unroll-buffer? x)
				   (->unroll-idx x nth)
				   x))
			   idx))))
		 buffer)))
    
    (alexandria:flatten
     (list
      (loop for nth upfrom 0 below unroll-by
	    collect
	    (make-uop-alu
	     :x-writes (list (->unroll-idx unroll-idx nth))
	     :x-reads  (list unroll-idx nth)
	     :op-type :+
	     :dtype   :int))
      (loop for op in uops
	    if (to-unroll? op)
	      collect
	      (typecase op
		(UOp-Load
		 ;; int val_0 = i;
		 ;; ->
		 ;; int val_0_1 = i+0;
		 ;; int val_0_2 = i+1;
		 ;;       ...
		 (push (uop-load-x1 op) seen)		 
		 (loop for nth upfrom 0 below unroll-by
		       if (equal (uop-load-reduction op) unroll-idx)
			 collect op
		       else
			 collect
			 (make-uop-load
			  :x1 (unroll-buffer (uop-load-x1 op) nth)
			  :x2 (unroll-buffer (uop-load-x2 op) nth)
			  :reduction (uop-load-reduction op))))
		(UOp-Store
		 (loop for nth upfrom 0 below unroll-by
		       if (equal (uop-store-reduction op) unroll-idx)
			 collect op
		       else
			 collect
			 (make-uop-store
			  :x1 (unroll-buffer (uop-store-x1 op) nth)
			  :x2 (unroll-buffer (uop-store-x2 op) nth)
			  :reduction (uop-store-reduction op))))
		(UOp-ALU
		 (dolist (w (uop-alu-x-writes op))
		   (push w seen))
		 (loop with writes = (uop-alu-x-writes op)
		       with reduction-p = (equal unroll-idx (uop-alu-reduction op))
		       for nth upfrom 0 below unroll-by
		       collect
		       (make-uop-alu
			:x-writes (if (equal unroll-idx (uop-alu-reduction op))
				      (uop-alu-x-writes op)
				      (map 'list #'(lambda (x) (unroll-buffer x nth)) (uop-alu-x-writes op)))
			:x-reads  (map
				   'list
				   #'(lambda (x)
				       (if (and reduction-p (find x writes :test #'equal))
					   x
					   (unroll-buffer x nth)))
				   (uop-alu-x-reads  op))
			:op-type  (uop-alu-op-type op)
			:dtype    (uop-alu-dtype op)
			:reduction (uop-alu-reduction op))))
		(UOp-Loop
		 (progn;;let ((loop-subbody (slice-loop-entity uops (range-id (uop-loop-iters op)))))
		   ;; TODO
		   ;; ループごと繰り返すように実装したい。(がテストするのがめんどくさい) Conv実装する時にでも。。。
		   (error "not ready!")
		   ))
		(T
		 (error "unroll-uops: add the case for unrolling ~a" op)
		 op))
	    else
	      collect op)))))

(defun %uopgraph-vectorize (graph idx scoping-type)
  "Attempts to vectorize the UOp codes"
  (declare (type UOpGraph graph))

  (assert (eql :vector (runtimeconfig-vectorize-strategy *runtime*)) () "%uops-vectorize is dedicated to a vector computor.")
  (assert (runtimeconfig-simd-len *runtime*) () "Failed to simdify the graph because
the current *runtime* do not know the number of SIMD_LEN: ~a" *runtime*)

  
  ;; Before we start a vectorization step, we need to confirm that tensors used inside the loop really have the same dtypes.
  ;; Otherwise, we cannot assert that all tensors are unrolled by the same number, failing to pack/unpack to simd register.
  
  (let ((loop-body (slice-loop-entity (uopgraph-uops graph) idx))
	(found-dtypes))
    (dolist (op loop-body)
      ;; [TODO] More UOps to check?
      (typecase op
	(UOp-Load
	 (with-slots ((x2 x2)) op
	   (when (aref-buffer-p x2)
	     (push (aref-buffer-name x2) found-dtypes))))
	(UOp-Store
	 (with-slots ((x1 x1)) op
	   (when (aref-buffer-p x1)
	     (push (aref-buffer-name x1) found-dtypes))))
	(UOp-ALU
	 (with-slots ((x-reads x-reads) (x-writes x-writes)) op
	   (dolist (x `(,@x-reads ,@x-writes))
	     (when (aref-buffer-p x)
	       (push (aref-buffer-name x) found-dtypes)))))))
    (setf found-dtypes (remove-duplicates found-dtypes :test #'equal))
    (when (not
	   (and (car found-dtypes)
		(every #'(lambda (x) (eql (aten/ir:aten-type-class (car found-dtypes)) (aten/ir:aten-type-class x))) found-dtypes)))
      (with-debug-level (2)
	(warn "%uops-vectorize: To vectorize uops with idx=~a, these tensors must be declared to have the same dtypes. ~%~a~%Proceeds with ignoring this vectorization." idx found-dtypes))
      (return-from %uopgraph-vectorize nil))

    ;; Rewriting LOAD/STORE/ALU using %uopgraph-unroll
    (let ((blocksize (/ (runtimeconfig-simd-len *runtime*) (dtype-size (aten/ir:aten-type-class (car found-dtypes))))))
      (assert (integerp blocksize) () "Assertion Failed with: (/ SIMD_LEN Dtype_Size) == Integer. butgot =~a" blocksize) 
      (%uopgraph-unroll graph idx blocksize scoping-type :unroller #'vectorize-uops))))


