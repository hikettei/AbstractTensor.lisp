
(in-package :abstracttensor/engine)

(deftype Operators ()
  `(and
    keyword
    (member
     :+
     :-
     :*
     :/
     :<
     :<=
     :>
     :>=

     :mod
     :floordiv ;; A // B, a.k.a: (floor X Y)

     ;; Special Ops
     :muladd ;; muladd(A, B, C) = A * B + C

     ;; Mathematical Functions (WIP)
     :exp     
     )))

