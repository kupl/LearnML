type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula

let rec eval : formula -> bool
=fun f -> match f with
| True -> true
| False -> false
| Neg a -> if a=True then false else true
| Or(a,b) -> if a=False && b=False then false else true
| And(a,b) -> if a=True && b=True then true else false
| Imply(a,b) -> if a=True && b=False then false else true
| Equiv(a,b) -> if a=b then true else false
