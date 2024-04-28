
(in-package :abstracttensor/ir)

;; Atoms = Principle Operators

;; add, sub, mul, div
;; math ops (sin cos etc)
;; move
;; cast
;; view

(defclass Lazy ()
  (belongs-to) ;; What composite belongs to?

  )

(defclass LazyMap    (Lazy) nil)
(defclass LazyReduce (Lazy) nil)
(defclass LazyCast   (Lazy) nil)
(defclass LazyView   (Lazy) nil)

;; ~ Definitions of each atoms ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
