
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
  | Neg f -> not (sat f)
  | And (f1, f2) -> (sat f1)&&(sat f2)
  | Or (f1, f2) -> (sat f1) || (sat f2)
  | Imply (f1, f2) -> if (sat f1) = false then true else (sat f2)
  | Iff (f1, f2) -> if (sat Imply f1, f2) = true && (sat Imply f2, f1) = true then true else false



