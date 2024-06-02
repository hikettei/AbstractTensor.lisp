
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine
  (:use :cl)
  (:nicknames aten/engine)

  ;; Composite
  (:export
   #:Compiled-Composite
   #:make-compiled-composite
   #:load-compiled-composite
   #:cc-runtime
   #:cc-base-composite
   #:cc-caller
   #:cc-defun-header
   #:cc-output-positions
   #:call
   #:test-composite
   #:*test-composite-mode*)
  
  ;; Types
  (:export
   #:Dtypes
   #:Operators
   )
  ;; UI
  (:export
   #:initialize-runtime
   #:declare-runtime
   #:realize
   #:*lazy-cache-buffer*
   #:*lazy-compile-mode*
   #:compile-source-code
   #:with-lazy-compile-mode
   )

  ;; UOpGraph
  (:export
   #:UOpGraph
   #:UOpGraph-uops
   #:render-graph
   #:render-buffer
   )
  
  (:export
   #:uop->buffer
   #:infer-buffer-type
   )

  ;; Simplifiers
  (:export
   #:uops-simplify
   #:define-simplifier
   )

  ;; Optimizations
  (:export
   #:uops-optimize
   )

  ;; UOps
  (:export
   #:uopcase
   #:buffercase)
  
  (:export
   #:Range
   #:make-range
   #:range-id
   #:range-from
   #:range-to
   #:range-by))

(in-package :abstracttensor/engine)


