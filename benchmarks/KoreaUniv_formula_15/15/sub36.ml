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
| Neg a -> if eval a=true then false else true
| Or (c,d) -> if eval c=false && eval d=false then false else true
| And (e,f) -> if eval e=true && eval f=true then true else false
| Imply (g,h) -> if eval g=true && eval h=false then false else true
| Equiv (i,j) -> if eval i=eval j then true else false;;
