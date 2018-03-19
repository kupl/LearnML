
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp
  
let rec calculatorHelper : exp * int -> int 
 = fun (exp, xValue) ->
	match exp, xValue with
	|X, xValue -> xValue
	|INT i, xValue -> i
	|ADD (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) + calculatorHelper (exp2, xValue)
	|SUB (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) - calculatorHelper (exp2, xValue)
	|MUL (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) * calculatorHelper (exp2, xValue)
	|DIV (exp1, exp2), xValue -> calculatorHelper (exp1, xValue) / calculatorHelper (exp2, xValue)
	|SIGMA (INT(a), INT (b), exp ), xValue -> 
			if a > b then raise (Failure "First Value of Sigma must be 		smaller or equal to the second value")
			else if a = b then calculatorHelper(exp, a)
			else calculatorHelper(exp, a) + calculatorHelper((SIGMA (INT(a+1), INT (b), exp)), xValue)
			
		
 
 let rec calculator : exp -> int 
 = fun exp ->
	match exp with
	|X -> raise (Failure "X is not defined")
	|INT i -> i
	|ADD (exp1, exp2) -> calculator exp1 + calculator exp2
	|SUB (exp1, exp2) -> calculator exp1 - calculator exp2
	|MUL (exp1, exp2) -> calculator exp1 * calculator exp2
	|DIV (exp1, exp2) -> calculator exp1 / calculator exp2
	|SIGMA (INT(a), INT (b), exp ) ->
			if a > b then raise (Failure "First Value of Sigma must be 		smaller or equal to the second value")
			else if a = b then calculatorHelper(exp, a)
			else calculatorHelper(exp, a) + calculator(SIGMA (INT(a+1), INT (b), exp))