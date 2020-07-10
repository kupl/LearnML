type formula = True | False | Not of formula | AndAlso of formula * formula | OrElse of formula * formula | Imply of formula * formula | Equal of exp * exp
and exp = Num of int | Plus of exp * exp | Minus of exp * exp

let rec  eval fml =
	match fml with
	|True -> true
	|False -> false
	|Not (fml') -> not (eval fml')
	|AndAlso (fml1, fml2) -> (eval fml1) && (eval fml2)  
	|OrElse (fml1, fml2) -> (eval fml1) || (eval fml1)
	|Imply (fml1, fml2) -> not(eval fml1) || (eval fml2) 
	|Equal (exp1, exp2) -> 
		let rec tr01 (exp1') =
			match exp1' with
			| Num (i) ->i 
		 	| Minus (ex1, ex2) -> tr01(ex1) - tr01(ex2)
			| Plus (ex1', ex2') -> tr01(ex1') + tr01(ex2')
		in
		let tr02 (exp00, exp01) =
			if tr01(exp00) = tr01(exp01) then true
			else false
		in
	tr02(exp1, exp2)
	 


