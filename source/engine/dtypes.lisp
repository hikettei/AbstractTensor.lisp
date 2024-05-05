
(in-package :AbstractTensor/engine)

(deftype Dtypes ()
  `(and
    keyword
    (member
     ;; Masking
     :bit
     
     ;; Integers
     :uint4
     :int4
     :uint8
     :int8
     :uint16
     :int16
     :uint32
     :int32
     :uint64
     :int64

     :int ;; Int32/64

     ;; Complex Numbers
     :complex64
     :complex128

     ;; Floats
     :FP16 ;; BFloat16 on CUDA, Half-float on CPU perhaps.
     :Float
     :Double

     ;; Possible?
     ;; :BFloat16

     ;; Boolenas
     :boolean
     )))

(defun ->lisp-type (dtype)
  (ecase dtype
    (:bit 'bit)
    (:uint4 '(unsigned-byte 4))
    (:int4  '(signed-byte 4))
    (:uint8 '(unsigned-byte 8))
    (:int8  '(signed-byte 8))
    (:uint16 '(unsigned-byte 16))
    (:int16  '(signed-byte 16))
    (:uint32 '(unsigned-byte 32))
    (:int32  '(signed-byte 32))
    (:uin64 '(unsigned-byte 64))
    (:int64  '(signed-byte 64))
    (:int 'integer)
    (:float 'single-float)
    (:double 'double-float)
    (:boolean 'boolean)))




