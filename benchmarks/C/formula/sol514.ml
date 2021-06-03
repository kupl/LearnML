(*Seok Jin Lee 2013-11417 CSE*)

type formula 	= True
		| False
		| Not of formula
		| AndAlso of formula * formula
		| OrElse of formula * formula
		| Imply of formula * formula
		| Equal of exp * exp

and exp	= Num of int
		| Plus of exp * exp
		| Minus of exp * exp

(*
let rec evalexp(exp: exp): int = 
	match exp with
	| Num n -> n
	| Plus(l,r) -> evalexp(l) + evalexp(r)
	| Minus(l,r) -> evalexp(l) - evalexp(r)
*)

let rec eval(f: formula): bool =
	let rec evalexp(exp: exp): int = 
		match exp with
		| Num n -> n
		| Plus(l,r) -> evalexp(l) + evalexp(r)
		| Minus(l,r) -> evalexp(l) - evalexp(r)
	
	in 
	match f with
	| True -> true
	| False -> false
	| Not op -> not(eval(op))
	| AndAlso(l,r) -> eval(l) && eval(r)
	| OrElse(l,r) -> eval(l) || eval(r)
	| Imply(l,r) -> not(eval(l)) || eval(r)
	| Equal(l,r) -> evalexp(l) = evalexp(r) 


