
;;
;;
;;

(in-package :abstracttensor/engine)


;; Reading list for myself:
;;  - https://github.com/Leslie-Fang/GEMM_Optimization/blob/master/optimization8/optimization8.cpp

	    
;; [TODO] あえて最初は冗長に記述して，後からSimplifierで最適化する
;; [MEMO] Unrollと混ざって考えるせいで，コンパイル時にPackedBufferかUnrolLEDBufferかわからない。
;; infer-type-engineみたいなのを作成して毎回TypeInferするべきか？？？
;; or Packedしか存在しないという仮定をおきたい

(defun prefix-p (prefix name)
  (if (>= (length name) (length prefix))
      (string= prefix (subseq name 0 (length prefix)))
      nil))

;; WMMA 2D Grouping
;;
;;                 N
;;             [ A B C ]
;;             [ D E F ]
;;             [ G H D ]
;;   [ A B C ]             [ r1 ]
;; M [ D E F ]   WMMA   => [ r2 ]
;;   [ G H D ]             [ r3 ]
;;

;; Conditions:
;; { M is read one-by-one
;; { N is unrolled
;; or
;; { M is unrolled
;; { N is read one-by-one

(defun replace-uop-group (uops uops-to-remove replace-with)
  ;; simplifierni prune wo makasereba ok
  )

(defun %vectorize-uops (graph loop2simd)
  (declare (type UOpGraph graph)
	   (type hash-table loop2simd))
  (with-local-simplifiers
    (labels ((undo1 (x)
	       (gethash x (uopgraph-unrolled-buffer graph)))
	     (collect-unrolled (users)
	       (loop for u in users
		     if (some #'(lambda (x) (undo1 x)) (uop-reads u))
		       collect u))
	     (get-unrolled-loadgroup (uops name)
	       (let* ((users (uops/value->users uops name)))
		 (collect-unrolled users))))
      (define-simplifier VectorizeLoop (uops uops-full)
	(symbol-macrolet ((->failed (return-from VectorizeLoop nil)))
	  (when (not (uop-loop-p (car uops)))->failed)
	  (let* ((key   (find (car uops) (alexandria:hash-table-keys loop2simd) :test #'equal))
		 (value (block form
			  (maphash #'(lambda (k v) (when (equal k key) (return-from form v))) loop2simd)))
		 (seen))
	    (when (null key)->failed)
	    (when (null value)->failed)
	    ;; (print key)
	    ;; (print value)
	    
	    ;; The earlier loop is sliced
	    
	    ;; 1 by 1 ならsimplify する				  
	    ;; ALU exists inside subject?
	    ;; Childを再起的に探索して置き換えていく
	    (let* ((packed) ;; ( (groups) . replacement)
 		   (loop-body (slice-loop-entity uops (range-id (uop-loop-iters (car uops)))))
		   (rest (subseq uops (length loop-body)))
		   (unroll-loads (or
				  (get-unrolled-loadgroup uops (range-id (uop-loop-iters (car uops))))				 
				  ->failed))
		   (_ (push (cons unroll-loads (vectorize-uop graph unroll-loads)) packed)))
	      (declare (ignore _))
	      (when (not (= value (length unroll-loads)))->failed)
	      (labels ((helper (prev-groups)
			 (let ((groups
				 (map
				  'list
				  #'(lambda (x)
				      (let ((out (collect-unrolled (uops/value->users loop-body (or (uop->buffer x) x)))))
					(loop for o in out
					      unless (find o seen :test #'equal) collect o)))
				  prev-groups)))
			   (unless (every #'(lambda (x) (= (length (car groups)) (length x))) groups)->failed)
			   (unless (= 0 (mod (length groups) value))->failed)
			   (loop for n upfrom 0 below (length (car groups))
				 for set = (loop for g in groups collect (nth n g))
				 do (push (cons set (vectorize-uop graph set)) packed)
				    (dolist (s set) (push s seen))
				    (helper (apply #'append (map 'list #'uop-writes set) (map 'list #'uop-reads set)))))))
		(helper (caar packed)))
	      `(,@(loop for u in uops
			collect
			(or
			 (loop named f
			       for (set . replacement) in packed
			       if (find u set :test #'equal)
				 do (return-from f replacement))
			 u))
		,@rest)				
	      ))))
      ;;(%uopgraph-simplify graph)
      )))


;; TODO Update

(defun vectorize-uop (graph group-uop)
  (labels ((undo1 (name)
	     (declare (type string name))
	     ;; return: (from (name nth))
	     (gethash name (uopgraph-unrolled-buffer graph)))
	   (make-const-aten (name type)
	     (aten/ir::make-aten name type nil nil nil))
	   (pack-buffer (buffer)
	     (when (not (typep buffer 'Buffers))
	       ;; 1とか入ってたら？
	       (return-from pack-buffer buffer))
	     (buffercase
	      buffer
	      :string ((value)
		       ;; _alu_0_5
		       ;; -> _packed_alu_0
		       (if (car (undo1 value))
			   (format nil "_packed_~a" (car (undo1 value)))
			   value))
	      :const  ((value type pointer-p)
		       ;; one of
		       ;; packedX + 1
		       ;; => packedX + {1, 1, 1, 1 ...}

		       ;; packedX + packedValue
		       ;; => packedX + packedValue
		       
		       (make-packed-buffer
			:packed-objects (loop repeat (length group-uop) collect value)
			:dtype type))
	      :aref ((name idx)
		     ;; name is a pointer
		     ;; idx is a scalar
		     ;; A[i, j]
		     ;; {A[i_0, j_0], A[i_1, j_0]}
		     (make-packed-buffer
		      :packed-objects
		      ;; 読んできた方向にしかpackしないといけないと思う・・・
		      (loop for op in group-uop
			    for nth upfrom 0
			    collect
			    (make-aref-buffer
			     :name name
			     :idx (loop for id in idx
					collect
					(if (undo1 id)
					    (make-aref-buffer
					     :name (make-const-aten (car (undo1 id)) (aten/ir::aten-type-class name))
					     :idx  (list nth))
					    id))))
		      :dtype (aten/ir::aten-type-class name)))
	      :packed ((packed-object dtype)
		       (make-packed-buffer
			:packed-objects packed-object
			:dtype dtype)))))
    
    (typecase (car group-uop)
      (UOp-Load
       (make-uop-load
	:x1 (pack-buffer (uop-load-x1 (car group-uop)))
	:x2 (pack-buffer (uop-load-x2 (car group-uop)))
	:reduction (uop-load-reduction (car group-uop))))
      (UOp-Store
       (make-uop-store
	:x1 (pack-buffer (uop-store-x1 (car group-uop)))
	:x2 (pack-buffer (uop-load-x2 (car group-uop)))
	:reduction (uop-store-reduction (car group-uop))))
      (UOp-ALU
       (make-uop-alu
	:x-writes (map 'list #'pack-buffer (uop-alu-x-writes (car group-uop)))
	:x-reads  (map 'list #'pack-buffer (uop-alu-x-reads (car group-uop)))
	:op-type  (uop-alu-op-type (car group-uop))
	:dtype    (uop-alu-dtype (car group-uop))
	:reduction (uop-alu-reduction (car group-uop))))
      (T (error "vectorize: not implemented: ~a" (car group-uop))))))

#|
(defun vectorize-uops (uops-full uops simd-idx n-pack &aux (seen (list simd-idx)))
  (labels ((->unroll-idx (name nth)
	     (if (equal (aref name 0) #\_)
		 (format nil "~a_~a" name nth)
		 (format nil "_~a_~a" name nth)))
	   (packed-name (name)
	     (if (equal (aref name 0) #\_)
		 (format nil "__packed_~(~a~)" name)
		 (format nil "__packed_~(~a~)" name)))
	   (unroll-name (name nth)
	     (if (equal (aref name 0) #\_)
		 (format nil "~a_~a" name nth)
		 (format nil "_~a_~a" name nth)))
	   (to-pack?     (uop) (intersection seen (uop-reads uop) :test #'equal))
	   (pack-buffer? (name)
	     (let ((name (if (aref-buffer-p name)
			     (format nil "~a" (aten/ir:aten-id (aref-buffer-name name)))
			     name)))
	       (find name seen :test #'equal)))
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
	   (unroll-packed-buffer (buffer nth)
	     (if (and
		  (packed-buffer-p buffer)
		  (stringp (car (packed-buffer-packed-objects buffer)))
		  (every #'(lambda (x) (string= x (car (packed-buffer-packed-objects buffer)))) (packed-buffer-packed-objects buffer))
		  (pack-buffer? (car (packed-buffer-packed-objects buffer))))
		 (packed-name (car (packed-buffer-packed-objects buffer)))
		 (if (and
		      (packed-buffer-p buffer)
		      (every #'aref-buffer-p (packed-buffer-packed-objects buffer))
		      (let ((reads (map 'list #'aref-buffer-idx (packed-buffer-packed-objects buffer))))
			(every (alexandria:compose #'pack-buffer? #'car) reads)))
		     (make-packed-buffer
		      :packed-objects (loop for i upfrom 0
					    for bf in (packed-buffer-packed-objects buffer)
					    collect
					    (print (make-aref-buffer :name (aref-buffer-name bf) :idx (unroll-buffer (print (aref-buffer-idx bf)) i))))
		      :dtype (packed-buffer-dtype buffer))
		     (make-packed-buffer
		      :packed-objects (map 'list #'(lambda (bf) (unroll-buffer bf nth)) (packed-buffer-packed-objects buffer))
		      :dtype (packed-buffer-dtype buffer)))))
	   (pack-buffer (buffer)
	     (buffercase
	      buffer
	      :string ((value)
		       (let ((type (infer-type-from-uop uops-full value)))
			 (if (pack-buffer? value) ;; ここで _val_7がTになるようにしたい
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
		       (error "Assertion Failed"))
	      :aref ((name idx)
		     (if (pack-buffer? (format nil "~a" (aten/ir:aten-id name)))
			 (pack-name (aten/ir:aten-id name))
			 (make-packed-buffer
			  :dtype (aten/ir:aten-type-class name)
			  :packed-objects
			  (loop for nth-packed upfrom 0 below n-pack
				collect
				(make-aref-buffer
				 :name name
				 :idx (loop for id in idx
					    if (pack-buffer? id)
					      collect (make-aref-buffer
						       :name
						       (aten/ir::make-aten
							(intern (packed-name id))
							(aten/ir::aten-type-class name)
							`(,simd-idx)
							`(0)
							nil)
						       :idx (list nth-packed))
					    else
					      collect id))))))))
	   (unroll-buffer (buffer nth)
	     (if (typep buffer 'buffers)
		 (buffercase
		  buffer
		  :string ((value)
			   (if (pack-buffer? value)
			       (->unroll-idx value nth)
			       value))
		  :const  ((value type pointer-p)
			   (if (pack-buffer? value)
			       (make-const-buffer :value (->unroll-idx value nth) :type type :pointer-p pointer-p)
			       value))
		  :aref ((name idx)
			 (make-aref-buffer
			  :name name
			  :idx
			  (map
			   'list
			   #'(lambda (x)
			       (if (pack-buffer? x)
				   (if (aref-buffer-p x)
				       (make-aref-buffer
					:name (aten/ir::make-aten (intern (->unroll-idx (format nil "~a" (aten/ir:aten-id (aref-buffer-name x))) nth)) (aten/ir:aten-type-class (aref-buffer-name x)) `(,n-pack) `(0) nil)
					:idx  (aref-buffer-idx x))
				       (->unroll-idx x nth))
				   x))
			   idx))))
		 buffer)))
    
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
		   (with-slots ((x1 x1) (x2 x2)) op
		     (if (equal (uop-load-reduction op) simd-idx)
			 op
			 (if (packed-buffer-p x2)
			     (loop for nth upfrom 0 below n-pack
				   collect
				   (make-uop-load
				    :x1 (unroll-name x1 nth)
				    :x2 (unroll-packed-buffer x2 nth)
				    :reduction (uop-load-reduction op)))
			     (make-uop-load
			      :x1 (pack-name   x1)
			      :x2 (pack-buffer x2)
			      :reduction (uop-load-reduction op))))))
		  (UOp-Store		 
		   (with-slots ((x1 x1) (x2 x2)) op
		     (if (equal (uop-store-reduction op) simd-idx)
			 op			 
			 (if (packed-buffer-p x1)
			     (loop for nth upfrom 0 below n-pack
				   collect
				   (make-uop-store
				    :x1 (unroll-packed-buffer x1 nth)
				    :x2 (unroll-name x2 nth)
				    :reduction (uop-store-reduction op)))
			     (make-uop-store
			      :x1 (pack-buffer x1)
			      :x2 (pack-name x2)
			      :reduction (uop-store-reduction op))))))
		  (UOp-ALU
		   (dolist (w (uop-alu-x-writes op))
		     (push w seen))
		   (with-slots ((x-reads x-reads) (x-writes x-writes)) op
		     (if (or (some #'packed-buffer-p x-reads) (some #'packed-buffer-p x-writes))
			 (loop for nth upfrom 0 below n-pack
			       collect
			       (make-uop-alu
				:x-writes
				(if (equal simd-idx (uop-alu-reduction op))
				    (uop-alu-x-writes op)
				    (loop for x in x-writes
					  if (packed-buffer-p x)
					    collect (unroll-packed-buffer x nth)
					  else
					    collect (unroll-buffer x nth)))
				:x-reads
				(loop for x in x-reads
				      collect
				      (if (and (equal simd-idx (uop-alu-reduction op)) (find x (uop-alu-x-writes op) :test #'equal))
					  x
					  (if (packed-buffer-p x)
					      (unroll-packed-buffer x nth)
					      (unroll-buffer x nth))))
 				:op-type (uop-alu-op-type op)
				:dtype   (uop-alu-dtype op)
				:reduction (uop-alu-reduction op)))
			 (make-uop-alu
			  :x-writes
			  (if (equal simd-idx (uop-alu-reduction op))
			      (uop-alu-x-writes op)
			      (loop for x in x-writes
				    collect
				    (pack-buffer x)))
			  :x-reads
			  (loop for x in x-reads
				collect
				(if (and (equal simd-idx (uop-alu-reduction op)) (find x (uop-alu-x-writes op) :test #'equal))
				    x
				    (pack-buffer x)))
			  :op-type   (uop-alu-op-type op)
			  :dtype     (uop-alu-dtype op)
			  :reduction (uop-alu-reduction op)))))
		  (UOp-Loop
		   (progn
		     (error "not ready! UOp-Loop[Vectorize]")
		     ))
		  (T
		   (error "unroll-uops: add the case to simdify ~a" op)
		   op))
	      else
		collect op))))))
|#

(defun %uopgraph-blocksize (graph idx)
  "Attempts to vectorize the UOp codes"
  (declare (type UOpGraph graph))
  (symbol-macrolet ((->failed (return-from %uopgraph-blocksize nil)))
    (macrolet ((try (condition message &rest more)
		 `(unless ,condition (progn (with-debug-level (3) (warn ,message ,@more)) (return-from %uopgraph-blocksize)))))
      (unless (eql :vector (runtimeconfig-vectorize-strategy *runtime*))->failed)
      (try (runtimeconfig-simd-len *runtime*) "Failed to simdify the graphh because the current *runtime* does not provide :SIMD_LEN: ~a" *runtime*)
      (try (eql (runtimeconfig-indexing-rule *runtime*) :flatten) "Failed to vectorize: indexing-rule must be set to :flatten to use a vectorization.")
      
      ;; Before we start a vectorization step, we need to confirm that tensors used inside the loop really have the same dtypes.
      ;; Otherwise, we cannot assert that all tensors are unrolled by the same number, failing to pack/unpack to simd register.

      (let ((loop-body (slice-loop-entity (uopgraph-uops graph) idx))
	    (found-dtypes))
	(dolist (op loop-body)
	  ;; [TODO] More UOps to check?
	  (typecase op
	    (UOp-Load
	     (with-slots ((x2 x2)) op
	       (when (packed-buffer-p x2)
		 (push (packed-buffer-dtype x2) found-dtypes))
	       (when (aref-buffer-p x2)
		 (push (aten/ir:aten-type-class (aref-buffer-name x2)) found-dtypes))))
	    (UOp-Store
	     (with-slots ((x1 x1)) op
	       (when (packed-buffer-p x1)
		 (push (packed-buffer-dtype x1) found-dtypes))
	       (when (aref-buffer-p x1)
		 (push (aten/ir:aten-type-class (aref-buffer-name x1)) found-dtypes))))
	    (UOp-ALU
	     (with-slots ((x-reads x-reads) (x-writes x-writes)) op
	       (dolist (x `(,@x-reads ,@x-writes))	     
		 (when (packed-buffer-p x)
		   (push (packed-buffer-dtype x) found-dtypes))
		 (when (aref-buffer-p x)
		   (push (aten/ir:aten-type-class (aref-buffer-name x)) found-dtypes)))))))
	(setf found-dtypes (remove-duplicates found-dtypes)
	      found-dtypes (loop for dtype in found-dtypes
				 if (not (eql dtype :int))
				   collect dtype)
	      found-dtypes (or found-dtypes `(:int))) ;; If found-dtypes=nil -> the iteration is dedicated to :int arrays.
	(when (not
	       (and (car found-dtypes)
		    (every #'(lambda (x) (eql (car found-dtypes) x)) found-dtypes)))
	  (with-debug-level (2)
	    (warn "%uops-vectorize: To vectorize uops with idx=~a, these tensors must be declared to have the same dtypes. ~%~a~%Proceeds with ignoring this vectorization." idx found-dtypes))
	  ->failed)

	;; Rewriting LOAD/STORE/ALU using %uopgraph-unroll
	(let ((blocksize (/ (runtimeconfig-simd-len *runtime*) (dtype-size (car found-dtypes)))))
	  (try (integerp blocksize) "Failed to vectorize the loop because it will not satisfies (/ SIMD_LEN Dtype_Size) == Integer. butgot =~a" blocksize) 
	  blocksize)))))


