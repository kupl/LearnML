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

let rec  sat : formula -> bool
= fun f -> match f with False -> false | True -> true | Neg b -> not (sat b) | And (b, c) -> sat b && sat c | Or (b,c) -> sat b || sat c | Imply (b,c) -> if sat b then sat c else true | Iff (b,c) -> if sat b then sat c else not (sat b) | Var b -> true
