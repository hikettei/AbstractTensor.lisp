
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine
  (:use :cl)
  (:nicknames aten/engine)

  ;; UI
  (:export
   #:declare-runtime
   #:realize
   )

  ;; UOpGraph
  (:export
   #:UOpGraph
   #:UOpGraph-uops
   #:render-graph
   #:render-buffer)
  
  (:export
   #:uop->buffer
   
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


