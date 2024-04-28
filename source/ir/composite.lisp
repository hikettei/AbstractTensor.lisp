
(in-package :abstracttensor/ir)

(defstruct Composite
  (documentation "" :type string)
  (path "" :type string)
  (name "" :type string)
  (inputs nil :type list)
  (outputs nil :type list)
  (code "" :type string))

