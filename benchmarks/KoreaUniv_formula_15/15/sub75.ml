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
    | Neg prop -> not (eval prop)
    | Or (prop1, prop2) -> eval prop1 || eval prop2
    | And (prop1, prop2) -> eval prop1 && eval prop2
    | Imply (prop1, prop2) -> not (eval prop1) || eval prop2
    | Equiv (prop1, prop2) -> eval prop1 = eval prop2
