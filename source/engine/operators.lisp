
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
     :sin
     :cos
     :tan
     :sinh
     :cosh
     :tanh
     :asinh
     :acosh
     :atanh

     :log
     :exp
     :sqrt
     )))

