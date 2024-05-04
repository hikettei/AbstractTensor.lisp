
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

     ;; Special Ops
     :muladd ;; muladd(A, B, C) = A * B + C

     ;; Mathematical Functions (WIP)
     :exp)))


