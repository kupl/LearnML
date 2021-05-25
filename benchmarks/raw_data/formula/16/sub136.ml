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

let rec eval : formula -> bool
	= fun f -> 
			match f with
			| True -> true
			| False -> false
			| Not x -> not (eval x)
									(*else raise (Failure "Type error : condition must be Bool type")*)
			| AndAlso (f1,f2) -> eval f1 && eval f2
			| OrElse (f1,f2) -> eval f1 || eval f2
			(*| Equal (f1,f2) -> if (f1=True && f2=True) then true
												else if (f1=False && f2=False) then true
												else false*)
			| Imply (f1,f2) -> not (eval f1) || eval f2
											 (*if (f1=True && f2=True) then eval True
											else if (f1=True && f2=False) then eval True
											else if (f1=False && f2=True) then eval False
											else if (f1=False && f2=False) then eval False
											else raise (Failure "Type Error : condition must be Bool type");;*)
			| Equal (f1,f2) -> let rec parseInt = fun x -> match x with
				| Num a -> a
				| Plus (f1,f2) -> (parseInt f1) + (parseInt f2)
				| Minus (f1,f2) -> (parseInt f1) - (parseInt f2) in (parseInt f1)=(parseInt f2);;




