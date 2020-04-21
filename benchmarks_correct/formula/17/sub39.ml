type formula = 	True
		| False
		| Not of formula
		| AndAlso of formula * formula
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp

and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp
(*
let rec calc (e: exp) : int
	match e with
	| Num n -> n
	| Plus (e1, e2) -> calc(e1) + calc(e2)
	| Minus of (e1, e2) -> calc(e1) - calc(e2)
*)

let rec eval (f: formula) : bool =
	match f with
	| True -> true
	| False -> false
	| Not fn -> not(eval fn)
	| AndAlso (f1, f2) -> (eval f1) && (eval f2)
	| OrElse (f1, f2) -> (eval f1) || (eval f2)
	| Imply (f1, f2) -> not(eval f1) || (eval f2)
	| Equal (e1, e2) -> 
		let rec calc (e: exp) : int =
			match e with
			| Num n -> n
			| Plus (e1, e2) -> (calc e1) + (calc e2)
			| Minus (e1, e2) -> (calc e1) - (calc e2) in
		if (calc e1) = (calc e2) then true else false
