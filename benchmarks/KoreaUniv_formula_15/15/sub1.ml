type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> 
match f with
True -> true
| False -> false
| Neg(x) -> not (eval x)
| Or(x,y) -> (eval x) || (eval y)
| And(x,y) -> (eval x) && (eval y)
| Imply(x,y) -> not (eval x) || (eval y)
| Equiv(x,y) -> (eval x) = (eval y)
