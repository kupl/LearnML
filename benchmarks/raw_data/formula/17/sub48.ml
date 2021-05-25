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

let rec eval_number f =
	match f with
	| Num f -> f
	| Plus(f1, f2) -> (eval_number f1)+(eval_number f2)
	| Minus(f1, f2) -> (eval_number f1)-(eval_number f2)
	
let rec eval_formula (f:formula) = 
	match f with
	| True -> True
	| False -> False
	| Not True -> False
	| Not False -> True
	| Not (f1) -> eval_formula(Not (eval_formula f1))
	| AndAlso(True, True) -> True
	| AndAlso(False, _) -> False
	| AndAlso(_, False) -> False
	| AndAlso((f1), (f2)) -> eval_formula(AndAlso((eval_formula f1), (eval_formula f2)))
	| OrElse(True, _) -> True
	| OrElse(_, True) -> True
	| OrElse(False, False) -> False
	| OrElse((f1), (f2)) -> eval_formula(OrElse((eval_formula f1), (eval_formula f2)))
	| Imply(True, True) -> True
	| Imply(True, False) -> False
	| Imply(False, _) -> True
	| Imply((f1), (f2)) -> eval_formula(Imply((eval_formula f1), (eval_formula f2)))
    | Equal((f1), (f2)) -> if ((eval_number f1)= (eval_number f2)) then True
    									else False
let eval (f:formula)=
	if((eval_formula f) == True) then true
	else false

