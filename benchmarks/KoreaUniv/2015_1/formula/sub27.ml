(* Problem 4 *)
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
  | True -> true
  | False -> false
  | Neg(f) -> if (eval f) then false else true
  | Or(f1, f2) -> if ((eval f1) || (eval f2)) then true else false
  | And(f1, f2) -> if ((eval f1) && (eval f2)) then true else false
  | Imply(f1, f2) -> if ((eval (Neg(f1))) || (eval f2)) then true else
    false
  | Equiv(f1, f2) -> if (eval f1) = (eval f2) then true else false
;;
