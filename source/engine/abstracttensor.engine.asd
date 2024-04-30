
(asdf:defsystem "abstracttensor.engine"
  :description "JIT Compiler and Runtime Engine for AbstractTensor"
  :author "hikettei <ichndm@gmail.com>"
  :licence "MIT"
  :serial t
  :components
  ((:file "package")

   ;; *uops must be loaded first* because when this file initialized
   ;; it also creates *uop-features*, and *buffer-features*
   ;; and, runtime.lisp depends on it. (when compile-toplevel)
   
   (:file "uops")
   (:file "schedule")
   (:file "runtime")
   (:file "uops-simplifier")
   (:file "uops-optimizer")
   (:file "uops-linearizer"))
  :in-order-to ((test-op (asdf:test-op "abstracttensor.engine/test"))))

(asdf:defsystem "abstracttensor.engine/test"
  :depends-on
  ("rove" "abstracttensor.lang" "abstracttensor.ir")
  :components
  ((:file "test-suites"))
  :perform
  (test-op (o s)
	   (uiop:symbol-call (find-package :rove) :run-suite :abstracttensor/engine.test)))
