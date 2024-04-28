
(asdf:defsystem "abstracttensor.lang"
  :description "A Programming Language dedicated to Deep Learning Compiler"
  :author      "hikettei <ichndm@gmail.com>"
  :licence     "MIT"
  :depends-on ("clop" "alexandria" "trivia")
  :components
  ((:file "package")
   (:file "trace")
   (:file "composite")
   ))

