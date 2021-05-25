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
		Num n -> n
		| Plus(n0,n1) -> (calc n0) + (calc n1)
		| Minus(n0,n1) -> (calc n0) - (calc n1)
	in

	match f with
	True -> true 
	| False -> false
	| Not f0 -> not (eval f0)
	| AndAlso(f0,f1) -> (eval f0) && (eval f1)
	| OrElse(f0,f1) -> (eval f0) || (eval f1)
	| Imply(f0,f1) -> (match (eval f0, eval f1) with
						(_,true) -> true 
						| (true, false) -> false
						| (false, false) -> true)
	| Equal(e0,e1) -> (if (calc e0)=(calc e1) then true
					   else false)
