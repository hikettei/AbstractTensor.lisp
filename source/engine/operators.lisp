
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
     :wmma

     ;; Mathematical Functions (WIP)
     :exp     
     )))

