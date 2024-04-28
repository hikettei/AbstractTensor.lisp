
(cl:in-package :cl-user)

(defpackage :abstracttensor/lang
  (:use :cl)
  (:nicknames aten/lang)
  (:export
   #:composite-from-toml
   #:composite-from-file
   #:trace-uops
   )
  (:import-from :aten/engine
		#:Range
		#:make-range
		#:range-id
		#:range-from
		#:range-to
		#:range-by))


