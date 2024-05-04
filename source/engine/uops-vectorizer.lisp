
(in-package :abstracttensor/engine)

;; Reading list for myself:
;;  - https://github.com/Leslie-Fang/GEMM_Optimization/blob/master/optimization8/optimization8.cpp

(defun %uops-vectorize (graph)
  "Attempts to vectorize the UOp codes by doing"
  (declare (type UOpGraph graph))

  ;; ALU/LoadあたりをUnrolする
  ;; SinはUnrollできる？とか+はUnrollできる？とかをConfigに記載する
  

  )

