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
| Neg n -> if n = True then false else true
| Or (n1,n2) -> if n1 = False && n2 = False then false else true
| And (n1,n2) -> if n1 = True && n2 = True then true else false
| Imply (n1, n2) -> if n1 = True && n2 = False then false else true
| Equiv (n1, n2) -> if n1 = n2 then true else false
