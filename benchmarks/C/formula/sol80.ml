type formula = True | False
	|Not of formula
	|AndAlso of formula * formula
	|OrElse of formula * formula
	|Imply of formula * formula
	|Equal of exp * exp
and exp = Num of int
	|Plus of exp * exp
	|Minus of exp * exp

let rec eval x =
	
	(* Calculate value *)
	let rec calexp t =
		match t with
		Num a-> a
		|Plus (a, b)->(calexp a) + (calexp b)
		|Minus (a, b)->(calexp a) - (calexp b)
	in


	match x with
	|True -> true
	|False -> false
	|Not a -> not (eval a)
	|AndAlso (a, b) -> (eval a)&&(eval b)
	|OrElse (a, b) -> (eval a)||(eval b)
	|Imply (a, b) -> (not (eval a))||(eval b)
	|Equal (a, b) -> if (calexp a)=(calexp b) then true else false
