
(asdf:defsystem "abstracttensor.engine"
  :description "JIT Compiler and Runtime Engine for AbstractTensor"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :serial t
  :depends-on
  ()
  :components
  ((:file "package")
   (:file "uops")
   (:file "schedule")))

