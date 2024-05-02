
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


     ;; Special Ops
     :muladd ;; muladd(A, B, C) = A * B + C

     ;; Mathematical Functions (WIP)
     :exp)))


