
(asdf:defsystem "abstracttensor.engine"
  :description "JIT Compiler and Runtime Engine for AbstractTensor"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :serial t
  :components
  ((:file "package")
   (:file "uops")
   (:file "schedule")
   (:file "uops-simplifier")
   (:file "uops-optimizer"))
  :in-order-to ((test-op (asdf:test-op "abstracttensor.engine/test"))))

(asdf:defsystem "abstracttensor.engine/test"
  :depends-on
  ("rove" "abstracttensor.lang" "abstracttensor.ir")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s)
	   (uiop:symbol-call (find-package :rove) :run-suite :abstracttensor/engine.test)))
