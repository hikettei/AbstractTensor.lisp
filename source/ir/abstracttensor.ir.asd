
(asdf:defsystem "abstracttensor.ir"
  :description "Elegant IR System for SoTA Deep Learning Compiler"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :serial t
  :depends-on
  ("cl-ppcre"
   "trivia"
   "xsubseq")
  :components
  ((:file "package")
   (:file "shape-tracker")
   (:file "atoms")
   (:file "composite")))

