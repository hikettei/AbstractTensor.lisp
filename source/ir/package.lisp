
(cl:in-package :cl-user)

(defpackage :AbstractTensor/IR
  (:nicknames aten/ir)
  (:documentation "
[TODO]

- [ ] ShapeTrackerを実装
    -  Compose
    - TODO: View, Reshape, Permute, Take(Slice)

- [ ] Composite
    - Principle Ops

- [ ] Composite Caching System
")
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
   #:composite-code
   #:composite-test-requirements
   #:composite-test-code))

