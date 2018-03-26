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
	| Neg (fnt) -> (not (eval (fnt)))
	| Or (fnt, snd) -> ((eval (fnt)) || (eval (snd)))
	| And (fnt, snd) -> ((eval (fnt)) && (eval (snd)))
	| Imply (fnt, snd) ->
		if (eval (fnt) = false) then true
		else if (eval (snd) = true) then true
		else false
	| Equiv (fnt, snd) ->
		if (eval (fnt) = eval (snd)) then true
		else false


