
(asdf:defsystem "abstracttensor"
  :description "General Purpose Deep Learning Compiler"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on
  ("abstracttensor.docs"
   "abstracttensor.ir"
   "abstracttensor.engine"
   "abstracttensor.lang"
   "cffi")
  :components
  ((:file "runtimes/clang"))
  :in-order-to
  ((test-op
    (asdf:test-op "abstracttensor.engine")
    ;; Other system follows
    )))

