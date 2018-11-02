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
= fun f -> match f with
| True -> true
| False -> false
| Var x -> if x = "P" then true else false (*preset P is true as the question has described*)
| Neg x -> if sat x = true then false else true
| And (e1, e2) -> sat e1 && sat e2
| Or (e1, e2) -> sat e1 || sat e2
| Imply (e1, e2)-> sat (Neg(e1)) || sat e2
| Iff (e1, e2) -> (sat (Neg(e1)) || sat e2) && (sat (Neg(e2)) || sat e1)