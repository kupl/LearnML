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

let rec expval (exp1:exp) : int =
							   match exp1 with
											   Num (n) -> n
											  |Plus(exp1,exp2) -> expval exp1 + expval exp2
											  |Minus(exp1,exp2) -> expval exp1 - expval exp2
let rec eval (f:formula) : bool =
					   match f with
					   True -> true
					  |False -> false
					  |Not (formula1) -> if(eval formula1 == true) then false else true
					  |AndAlso (formula1,formula2) -> if(eval formula1 == true && eval formula2 == true) then true else false
					  |OrElse (formula1,formula2) -> if(eval formula1 == true || eval formula2 == true) then true else false
					  |Imply (formula1,formula2) -> if(eval formula1 == true && eval formula2 == false) then false else true
					  |Equal (exp1,exp2) -> if(expval exp1 = expval exp2) then true else false

