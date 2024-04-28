
(cl:in-package :cl-user)

(defpackage :abstracttensor/engine
  (:use :cl)
  (:nicknames aten/engine)
  (:export
   #:uop->buffer
   
   )
  (:export
   #:Range
   #:make-range
   #:range-id
   #:range-from
   #:range-to
   #:range-by))


