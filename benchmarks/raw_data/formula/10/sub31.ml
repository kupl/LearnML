type formula  = True	
	| False
	| Not of formula
  	| AndAlso of formula * formula
  	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp
and exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec eval : formula -> bool = 
	let rec cal e = 
		match e with Num a -> a
			|Plus (e1,e2) -> (cal e1) + (cal e2)
			|Minus (e1,e2) -> (cal e1) - (cal e2) in
	fun f -> match f with
		True -> true
		|False -> false
		|Not a -> not (eval a)
		|AndAlso (f1,f2) -> (eval f1) && (eval f2)
		|OrElse (f1,f2) -> (eval f1) || (eval f2)
		|Imply (f1,f2) -> if ((eval f1) = true) && ((eval f2 = false)) then false
				  else true
		|Equal (e1,e2) -> (cal e1) = (cal e2)
