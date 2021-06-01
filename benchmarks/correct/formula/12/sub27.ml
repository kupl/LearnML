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

let rec calExpr exp =
	match exp with
	| Num i -> i
	| Plus (left, right) -> (calExpr left) + (calExpr right)
	| Minus (left, right) -> (calExpr left) - (calExpr right)

let rec eval fml =
	match fml with
	| True -> true
	| False -> false
	| Not subFml -> not (eval subFml)
	| AndAlso (fml1, fml2) -> (eval fml1) && (eval fml2)
	| OrElse (fml1, fml2)  -> (eval fml1) || (eval fml2)
	| Imply (fml1, fml2) 	-> if ((eval fml1)&&(eval fml2))|| (not (eval fml1)) then true else false
	| Equal (fml1, fml2)	-> if ((calExpr fml1) = (calExpr fml2)) then true else false 
