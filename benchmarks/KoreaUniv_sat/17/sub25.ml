(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula



let rec sat : formula -> bool
= fun f -> 
match f with 
| True -> true
| False -> false
| Var (s) -> true
| Neg (f) -> if (sat f) then false else true
| And (f1,f2) -> (sat f1)&&(sat f2)
| Or (f1,f2) -> (sat f1)||(sat f2)
| Imply (f1,f2) -> if (sat f1) then true else (sat f2)
| Iff (f1,f2) -> (sat f1) = (sat f2)				