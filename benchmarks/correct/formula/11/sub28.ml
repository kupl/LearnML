(* 2009-11824 Jieun-Jeong HW1-5 *)

type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval f =
	let rec calc e =
		match e with
		Num n		-> n
		|Plus (el, er)	-> (calc el) + (calc er)
		|Minus (el, er) -> (calc el) - (calc er)
	in
	match f with
	True			-> true
	|False			-> false
	|Not f			-> if (eval f) then false else true
	|AndAlso (fl, fr)	-> (eval fl) && (eval fr)
	|OrElse (fl, fr)	-> (eval fl) || (eval fr)
	|Imply (fl, fr)		-> (eval (Not fl)) || (eval fr)
	|Equal (el, er)		-> (calc el) = (calc er)	
