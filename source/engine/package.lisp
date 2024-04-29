
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine
  (:use :cl)
  (:nicknames aten/engine)
  (:export
   #:uop->buffer
   
   )
  (:export
   #:uops-simplify
   #:define-simplifier)
  
  (:export
   #:Range
   #:make-range
   #:range-id
   #:range-from
   #:range-to
   #:range-by))

(in-package :abstracttensor/engine)


