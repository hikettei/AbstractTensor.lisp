
;;
;;
;;

(in-package :abstracttensor/engine)


;; Reading list for myself:
;;  - https://github.com/Leslie-Fang/GEMM_Optimization/blob/master/optimization8/optimization8.cpp

	    
;; [TODO] あえて最初は冗長に記述して，後からSimplifierで最適化する
(defun vectorize-uops (uops-full uops simd-idx n-pack &aux (seen (list simd-idx)))
  (labels ((->unroll-idx (name nth)
	     (if (equal (aref name 0) #\_)
		 (format nil "~a_~a" name nth)
		 (format nil "_~a_~a" name nth)))
	   (packed-name  (name)
	     (if (equal (aref name 0) #\_)
		 (format nil "_packed~(~a~)" name)
		 (format nil "_packed_~(~a~)" name)))
	   (to-pack?     (uop) (intersection seen (uop-reads uop) :test #'equal))
	   (pack-buffer? (name) (find name seen :test #'equal))
	   (pack-name    (name) (if (pack-buffer? name) (packed-name name) name))
	   (repeat (obj type)
	     (if (pack-buffer? obj)
		 (progn
		   (assert (stringp obj) ())
		   (loop for nth upfrom 0 below n-pack
			 collect
			 (make-aref-buffer
			  :name (aten/ir::make-aten (intern obj) type (list n-pack) '(0) nil)
			  :idx  (list nth))))
		 (loop for nth upfrom 0 below n-pack
		       collect obj)))
	   (pack-buffer (buffer)
	     (buffercase
	      buffer
	      :string ((value)
		       (let ((type (infer-type-from-uop uops-full value)))
			 (if (pack-buffer? value)
			     (pack-name value)
			     (make-packed-buffer
			      :dtype type
			      :packed-objects (repeat value type)))))
	      :const  ((value type pointer-p)
		       (if (pack-buffer? value)
			   (make-const-buffer
			    :value (pack-name value)
			    :type type
			    :pointer-p pointer-p)			    
			   (make-packed-buffer
			    :dtype type
			    :packed-objects (repeat buffer type))))			   
	     :packed ((objects dtype)
		      (error "TODO???")
		      buffer)
	     :aref ((name idx)
		    (if (pack-buffer? (aten/ir:aten-id name))
			(pack-name (aten/ir:aten-id name))
			(make-packed-buffer
			 :dtype (aten/ir:aten-type-class name)
			 :packed-objects
			 (loop for nth-packed upfrom 0 below n-pack
			       collect
			       (make-aref-buffer
				:name name
				:idx (loop for id in idx
					   for nth_idx upfrom 0
					   if (pack-buffer? id)
					     collect (make-aref-buffer
						      :name
						      (aten/ir::make-aten
						       (intern (packed-name id))
						       (aten/ir::aten-type-class name)
						       `(,simd-idx)
						       `(0)
						       nil)
						      :idx (list nth_idx))
					   else
					     collect id)))))))))
					   
    (let ((packed-indices
	    (loop for nth upfrom 0 below n-pack
		  collect
		  (make-uop-alu
		   :x-writes (list (->unroll-idx simd-idx nth))
		   :x-reads  (list simd-idx nth)
		   :op-type :+
		   :dtype   :int))))
      (alexandria:flatten
       (list
	;; Unrolling range-idx in order to compute simd idx. (later simplified)
	packed-indices
	(make-uop-load
	 :x1 (pack-name (pack-name simd-idx))
	 :x2 (make-packed-buffer :dtype :int :packed-objects (map 'list (alexandria:compose #'car #'uop-alu-x-writes) packed-indices)))
	(loop for op in uops
	      if (to-pack? op)
		collect
		(typecase op
		  (UOp-Load
		   ;; int val_0 = i;
		   ;; ->
		   ;; int val_0_1 = i+0;
		   ;; int val_0_2 = i+1;
		   ;;       ...
		   (push (uop-load-x1 op) seen)
		   (if (equal (uop-load-reduction op) simd-idx)
		       op
		       (make-uop-load
			:x1 (pack-name (uop-load-x1 op))
			:x2 (pack-buffer (uop-load-x2 op))
			:reduction (uop-load-reduction op))))
		  (UOp-Store
		   (if (equal (uop-store-reduction op) simd-idx)
		       op
		       (make-uop-store
			:x1 (pack-buffer (uop-store-x1 op))
			:x2 (pack-name (uop-store-x2 op))
			:reduction (uop-store-reduction op))))
		  (UOp-ALU
		   (dolist (w (uop-alu-x-writes op))
		     (push w seen))
		   (make-uop-alu
		    :x-writes
		    (if (equal simd-idx (uop-alu-reduction op))
			(uop-alu-x-writes op)
			(loop for x in (uop-alu-x-writes op)
			      collect
			      (pack-buffer x)))
		    :x-reads
		    (loop for x in (uop-alu-x-reads op)
			  collect
			  (if (and (equal simd-idx (uop-alu-reduction op)) (find x (uop-alu-x-writes op) :test #'equal))
			      x
			      (pack-buffer x)))
		    :op-type   (uop-alu-op-type op)
		    :dtype     (uop-alu-dtype op)
		    :reduction (uop-alu-reduction op)))
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
		collect op))))))

(defun %uopgraph-vectorize (graph idx scoping-type)
  "Attempts to vectorize the UOp codes"
  (declare (type UOpGraph graph))

  (assert (eql :vector (runtimeconfig-vectorize-strategy *runtime*)) () "%uops-vectorize is dedicated to a vector computor.")
  (assert (runtimeconfig-simd-len *runtime*) () "Failed to simdify the graph because
the current *runtime* do not know the number of SIMD_LEN: ~a" *runtime*)  
  (assert (eql (runtimeconfig-indexing-rule *runtime*) :flatten)
	  ()
	  "Assertion failed: [vectorize-uops] indexing-rule must be :flatten to apply SIMDify")

  
  ;; Before we start a vectorization step, we need to confirm that tensors used inside the loop really have the same dtypes.
  ;; Otherwise, we cannot assert that all tensors are unrolled by the same number, failing to pack/unpack to simd register.
  (print idx)
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
    ;; Simplifier-Basedの実装の方が楽な気がしてきた。。。？
    (let ((blocksize (/ (runtimeconfig-simd-len *runtime*) (dtype-size (aten/ir:aten-type-class (car found-dtypes))))))
      (assert (integerp blocksize) () "Assertion Failed with: (/ SIMD_LEN Dtype_Size) == Integer. butgot =~a" blocksize) 
      (%uopgraph-unroll graph idx blocksize scoping-type :unroller #'vectorize-uops
			))))


