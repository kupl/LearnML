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
 True -> true
| False -> false
| Var x -> if true then true else false
| Neg e -> not (sat e)
| And (a,b) -> (sat a) && (sat b)
| Or (a,b) -> (sat a) || (sat b)
| Imply (a,b) -> not (sat a) || (sat b)
| Iff (a,b) -> (sat a) = (sat b);;