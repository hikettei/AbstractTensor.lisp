
(cl:in-package :cl-user)

(defpackage :AbstractTensor/IR
  (:nicknames aten/ir)
  (:use :cl)
  (:export
   #:AbstractTensor
   #:aten-id
   #:aten-type-class
   #:aten-shape
   #:aten-order
   #:aten-where
   #:parse-aten)
  (:export
   #:Composite
   #:make-composite
   #:composite-documentation
   #:composite-path
   #:composite-name
   #:composite-inputs
   #:composite-outputs
   #:composite-code))

