
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine
  (:use :cl)
  (:nicknames aten/engine)
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
   #:uop-optimize
   )
  
  (:export
   #:Range
   #:make-range
   #:range-id
   #:range-from
   #:range-to
   #:range-by))

(in-package :abstracttensor/engine)


