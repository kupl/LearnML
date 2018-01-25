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
	| Neg x-> if eval x then false else true
	| Or (x, y) -> if (eval x=true)||(eval y=true) then true else false
	| And (x, y) -> if (eval x=false)||(eval y=false) then false else true
	| Imply (x, y) ->
		(match eval x with
			|true -> eval y
			|false -> true)
	|Equiv (x, y) -> if (x=y) then true else false
	