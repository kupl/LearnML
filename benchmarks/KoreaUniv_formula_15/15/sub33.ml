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

let eval formula =
	match formula with
	  Neg f -> not f
	| Or(f1, f2) -> f1 || f2
	| And(f1, f2) -> f1 && f2
	| Imply (f1, f2) -> not(f1 && not f2)
	| Equiv (f1, f2) ->  if f1 then f2 else not f2;;
