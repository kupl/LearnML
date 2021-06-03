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



let rec expformula : exp -> int
= fun exp ->
	match exp with
	| Num (exp) -> exp
	| Plus (exp1, exp2) -> (expformula exp1) + (expformula exp2)
	| Minus (exp1, exp2) -> (expformula exp1) - (expformula exp2)
  


let rec eval : formula -> bool
= fun f -> 
	match f with
	| True -> true
	| False -> false
	| Not fm -> not (eval fm)
	| AndAlso (fm1, fm2) -> (eval fm1) && (eval fm2)
	| OrElse (fm1, fm2) -> (eval fm1) || (eval fm2)
	| Imply (fm1, fm2) -> if eval fm1 = false then true
	 									else if eval fm2 = true then true
										else false
	| Equal (exp1, exp2) -> if expformula(exp1) = expformula(exp2) 
														then true
													else false



