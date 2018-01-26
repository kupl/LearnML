(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


let rec calculator : exp -> int
  = fun exp -> 
match exp with
|X -> raise NotImplemented
|INT k -> k
|ADD (exp1,exp2) -> calculator exp1 + calculator exp2
|SUB (exp1,exp2) -> calculator exp1 - calculator exp2
|MUL (exp1,exp2) -> (calculator exp1) * (calculator exp2)
|DIV (exp1,exp2) -> (calculator exp1) / (calculator exp2)
|SIGMA (exp1,exp2,exp3) ->
if (calculator exp1)>(calculator exp2) then raise NotImplemented
else if (calculator exp1)<(calculator exp2) then 
(eval_sigma (calculator exp1) exp3) + 
calculator (SIGMA (INT (1+(calculator exp1)), exp2, exp3))
else eval_sigma (calculator exp2) exp3


and eval_sigma x exp3
= match exp3 with
|X->x
|INT k -> k
|ADD (exp1,exp2) -> (eval_sigma x exp1) + (eval_sigma x exp2)
|SUB (exp1,exp2) -> (eval_sigma x exp1) - (eval_sigma x exp2)
|MUL (exp1,exp2) -> (eval_sigma x exp1) * (eval_sigma x exp2)
|DIV (exp1,exp2) -> (eval_sigma x exp1) / (eval_sigma x exp2)
|_-> raise NotImplemented