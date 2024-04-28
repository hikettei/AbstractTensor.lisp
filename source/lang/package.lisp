
(cl:in-package :cl-user)

(defpackage :abstracttensor/lang
  (:use :cl)
  (:nicknames aten/lang)
  (:export
   #:composite-from-toml
   #:composite-from-file
   #:trace-uops
   ))


