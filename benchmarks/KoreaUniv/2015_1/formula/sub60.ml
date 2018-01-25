type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with True -> true
 | False -> false
 | Neg p when eval (p) = true -> false
 | Neg p when eval (p) = false -> true
 | Or (p,q) when eval (p) = true || eval (q) = true -> true
 | Or (p,q) when eval (p) = false && eval (q) = false -> false
 | And (p,q) when eval (p) = true && eval (q) = true -> true
 | And (p,q) when eval (p) = false || eval (q) = false -> false
 | Imply (p,q) when eval (p) = false && eval (q) = false -> true
 | Imply (p,q) when eval (p) = true && eval (q) = false -> false
 | Imply (p,q) when eval (q) = true -> true
 | Equiv (p,q) when eval (p) = eval (q) = true -> true
 | Equiv (p,q) when eval (p) = eval (q) = false -> false
