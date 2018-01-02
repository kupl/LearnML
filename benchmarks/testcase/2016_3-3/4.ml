type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec eval_sigma x exp3
= match exp3 with
	|X->x
	|INT k -> k
	|ADD (exp1,exp2) -> (eval_sigma x exp1) + (eval_sigma x exp2)
	|SUB (exp1,exp2) -> (eval_sigma x exp1) - (eval_sigma x exp2)
	|MUL (exp1,exp2) -> (eval_sigma x exp1) * (eval_sigma x exp2)
	|DIV (exp1,exp2) -> (eval_sigma x exp1) / (eval_sigma x exp2)

let rec calculator : exp -> int
= fun exp -> 
	match exp with
	|INT k -> k
	|ADD (exp1,exp2) -> calculator exp1 + calculator exp2
	|SUB (exp1,exp2) -> calculator exp1 - calculator exp2
	|MUL (exp1,exp2) -> (calculator exp1) * (calculator exp2)
	|DIV (exp1,exp2) -> (calculator exp1) / (calculator exp2)
	|SIGMA (exp1,exp2,exp3) ->
	if (calculator exp1)<(calculator exp2) then 
	(eval_sigma (calculator exp1) exp3) + 
	calculator (SIGMA (INT (1+(calculator exp1)), exp2, exp3))
	else eval_sigma (calculator exp2) exp3

