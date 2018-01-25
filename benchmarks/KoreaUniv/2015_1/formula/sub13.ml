(* Problem 4 *)
type formula =
    True
  | False
  | Neg of formula
  | Or of formula * formula
  | And of formula * formula
  | Imply of formula * formula
  | Equiv of formula * formula;;

let rec eval : formula -> bool
=fun f -> true;;

let rec eval what_formula = match what_formula with
|True -> true
|False -> false
| Neg formula -> if (formula = True) then false else true
| Or (formula1, formula2) -> if (formula1 = False && formula2 = False) then false else true
| And (formula1, formula2) -> if (formula1 = True && formula2 = True) then true else false
| Imply (formula1, formula2) -> if (formula1 = True && formula2 = False) then false else true
| Equiv (formula1, formula2) -> if (formula1 = formula2) then true else false;;
