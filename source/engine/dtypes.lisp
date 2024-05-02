
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


