
(in-package :abstracttensor/engine)

;;
;; TODO: Dynamic Shapeのままコンパイルできるようにする。
;;    - Dynamic Shapeで使う変数の依存関係をDAGが読めるようにする
;;    - Polyhedral Compilation
;;


(defun %uops-linearize (graph
			&key
			  
			  (dont-use-groups t))
  (declare (type UOpGraph graph))

  )
