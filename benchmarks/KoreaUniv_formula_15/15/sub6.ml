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
True -> true
|False -> false
|Neg f2 -> not (eval f2)
|Or (f2, f3) -> eval f2 || eval f3
|And (f2, f3) -> eval f2 && eval f3
|Imply (f2, f3) -> if eval f2 then eval f3 else true
|Equiv (f2, f3) -> if eval f2 = eval f3 then true else false;;
