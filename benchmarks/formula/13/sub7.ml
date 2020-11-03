type formula  = True
			| False
			| Not of formula
			| AndAlso of formula * formula
			| OrElse of formula * formula
			| Imply of formula * formula
			| Equal of exp * exp
and  exp = Num of int
		| Plus of exp * exp
		| Minus of exp * exp


let rec expval ex=
	match ex with
		|Num n -> n
		|Plus (ex1,ex2) -> expval(ex1) + expval(ex2)
		|Minus (ex1,ex2) -> expval(ex1) - expval(ex2)



	
let rec eval fm=
	match fm with 
		|True -> true
		|False -> false
		|Not f -> not (eval f)
		|AndAlso (fm1, fm2) -> (eval fm1) && (eval fm2)
		|OrElse (fm1, fm2) -> (eval fm1) || (eval fm2)
		|Imply (fm1, fm2) -> not ( (eval fm1) && not (eval fm2))
		|Equal (exp1, exp2) -> if (expval exp1) = (expval exp2) then true else false
