type formula =
	| True
	| False 
	| Not of formula 
	| AndAlso of formula * formula 
	| OrElse of formula * formula 
	| Imply of formula * formula 
	| Equal of exp * exp

and exp = 
	| Num of int 
	| Plus of exp * exp 
	| Minus of exp * exp 

let rec expf x = match x with 
	| Num n -> n
	| Plus (e1, e2) -> (expf e1) + (expf e2)
	| Minus (e1, e2) -> (expf e1) - (expf e2);;

let rec eval : formula -> bool
= fun f -> match f with
	|True -> true
	|False -> false
	|Not n -> (match eval n with
							|true -> false
							|false -> true)
	|AndAlso (n1, n2) -> (eval n1) && (eval n2)
	|OrElse (n1, n2) -> (match eval n1 with
													|true -> (match eval n2 with
																		|true -> true
																		|false -> true)
													|false -> (match eval n2 with
																		|true -> true
																		|false -> false))
	|Imply (n1, n2) -> (match eval n1 with
													|true -> (match eval n2 with
																		|true -> true
																		|false -> false)
													|false -> (match eval n2 with
																		|true -> true
																		|false -> true))
	|Equal (n1, n2) -> 
		let a1 = expf n1 in 
		let a2 = expf n2 in 
		if a1 == a2 then true else false;;
