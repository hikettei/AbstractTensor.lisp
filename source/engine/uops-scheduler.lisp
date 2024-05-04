
(in-package :abstracttensor/engine)

;; Utils for manipulating UOp-Loop and UOp-EndLoop
;; including
;;  - Unroll
;;  - Split
;;  - Reduce
;;  et al.


;; [TODO] SimplifierでAccumlationをループの外に出す最適化を実現したい。X += Yはreduce要の変数を作る的な感じで
;; [TODO] Dynamic ShapeはUnrollできるところはUnrollして。。。みたいな実装にしたい。



(defun %uopgraph-optimize-accumation (graph)
  (declare (type UOpGraph graph))
  (with-local-simplifiers
    (define-simplifier Accumlation->Reduction (uops uops-full)
      ;;
      ;; for (...) {
      ;;   Z = Z + a1 * a1
      ;; }
      ;; ->
      ;; float acc_n = 0;
      ;; for (...) {
      ;;   acc_n = a1 * a1
      ;; }
      ;; Z = acc_n

      ;; Zの依存がfor int kのやつになって, acc_n = Z*nみたいに表せる時だけ
      (symbol-macrolet ((->failed (return-from Accumlation->Reduction nil)))
	(when (not (uop-loop-p (car uops)))->failed)
	(let* ((uop-loop  (car uops))
	       (range-idx (range-id (uop-loop-iters uop-loop)))
	       (end-loop  (find range-idx (cdr uops) :test #'equal :key #'(lambda (x) (and (uop-endloop-p x) (range-id (uop-endloop-iters x)))))))
	  
	  (when (null end-loop)->failed)

	  ;; Only the innermost loops are the subject to this simplification.
	  (loop with ids = nil
		for uop in uops
		if (uop-loop-p uop)
		  do (push (range-id (uop-loop-iters uop)) ids)

		if (uop-endloop-p uop)
		  do (setf ids (remove (range-id (uop-endloop-iters uop)) ids :test #'equal))

		if (> (length ids) 1)
		  do (progn ->failed))

	  ;; a requirement for applying this simplification:
	  ;;  - Innermost Loop
	  ;;  - UOp-Store exists
	  ;;      - it depends on the computation inside the loop
	  ;;      - it does not depends on range-idx

	  (let* ((innerloop-uops
		   (loop named collector
			 for uop in (cdr uops)
			 if (uop-endloop-p uop)
			   do (return-from collector)
			 else
			   collect uop))
		 (stores-to-rewrite
		   (loop for uop in innerloop-uops
			 if (uop-store-p uop)
			   collect
			   (let* ((x1 (uop-store-x1 uop))
				  (x2 (uop-store-x2 uop)))
			     (when (aref-buffer-p x1)
			       ;; x1 must be independant from range-idx
			       (let ((parents
				       (loop for idx in (aref-buffer-idx x1)
					     append
					     (recursively-find-deps uops-full idx))))
				 (when (null (find range-idx parents :test #'equal))
				   ;; Finding ALU from parents
				   (let ((values (uops/user->values innerloop-uops x2)))
				     
				     ;; It is asserted that multiply-accumlation is fused by other simplifiers
				     ;; [TODO] More casse for simplification ops
				     ;; currently :muladd is only supported.
				     (when (and
					    (= (length values) 1)
					    (uop-alu-p (car values))
					    (eql :muladd (uop-alu-op-type (car values)))
					    ;; :muladd is used as an accumlation?
					    ;; e.g.: C = A * B + C

					    ;; Memo
					    ;; ここで C = A * B + Cの形になっているかを見てReduce条件が完成。。。が
					    ;; Aref BufferのIDXが同じものをさしているかわからない！
					    ;; Simplifierをもう少し頑張る必要がある。 (Done)
					    )
				       
				       )))))))))

	    ;; UOp-StoreをCollectしてそっから始める
	    ;; Storeを最終的に書き換える
	    ;; value->users nullが条件
	    ))))
    
    (%uopgraph-simplify graph)
    graph))

