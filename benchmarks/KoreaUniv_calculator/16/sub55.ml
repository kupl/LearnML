
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception Fault

let rec eval2 (exp, xval) =
	match (exp, xval) with
	 | (X, INT x) -> x
	 | (X, _) -> raise Fault 
	 | (INT i, _) -> i
	 | (ADD (exp1, exp2), _) ->
	 	(
		match exp1, exp2 with
		| INT x, INT y -> x+y
		| _, _ -> (eval2 (exp1, xval) + eval2 (exp2, xval))
		)
	| (SUB (exp1, exp2), _) ->
		(           
		match exp1, exp2 with                   
		| INT x, INT y -> x-y
		| _, _ -> (eval2 (exp1, xval) - eval2 (exp2, xval))
		)
	| (MUL (exp1, exp2), _) ->
		(
		match exp1, exp2 with                       
		| INT x, INT y -> x*y
		| _, _ -> (eval2 (exp1, xval) * eval2 (exp2, xval))
		)                                           
	| (DIV (exp1, exp2), _) ->                                                      
		(                                                                       
		match exp1, exp2 with                                                                                       
		| INT x, INT y -> x/y  
		| _, _ -> (eval2 (exp1, xval) / eval2 (exp2, xval))      
		)                                     
	| (SIGMA (exp1, exp2, exp3), _) ->
		let x = eval2 (exp1, xval)
		and y = eval2 (exp2, xval) in
		(
		if x>y then 0
		else if x=y then eval2 (exp3, INT x)
		else eval2 (exp3, INT x) + eval2 (SIGMA(INT (x+1), INT y, exp3), xval)
		)

  let calculator : exp -> int
  = fun exp -> eval2 (exp, X)