(* EXERCISE 1 *)
type exp = Num of int
	| Plus of exp * exp
	| Minus of exp * exp

let rec expeval : exp -> int = fun input ->
	match input with
	| Num (n) -> n
	| Plus (exp1, exp2) -> (expeval exp1) + (expeval exp2)
	| Minus (exp1, exp2) -> (expeval exp1) - (expeval exp2)

type formula = True
	| False
	| Not of formula
	| AndAlso of formula * formula
	| OrElse of formula * formula
	| Imply of formula * formula
	| Equal of exp * exp

let rec eval : formula -> bool = fun input ->
	match input with
	| True -> true
	| False -> false
	| Not (subf) -> not (eval subf)
	| AndAlso (subf1, subf2) -> (eval subf1) && (eval subf2)
	| OrElse (subf1, subf2) -> (eval subf1) || (eval subf2)
	| Imply (subf1, subf2) -> if (eval subf1) && (not (eval subf2)) then false
				else true
	| Equal (exp1, exp2) -> if (expeval exp1) = (expeval exp2) then true
				else false
(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval True); 
print_bool (false = eval False); 
print_bool (false = eval (Not True)); 
print_bool (true = eval (Not False)); 
print_bool (true = eval (AndAlso (True, True))); 
print_bool (false = eval (AndAlso (True, False))); 
print_bool (false = eval (AndAlso (False, True))); 
print_bool (false = eval (AndAlso (False, False))); 
print_bool (true = eval (OrElse (True, True))); 
print_bool (true = eval (OrElse (True, False))); 
print_bool (true = eval (OrElse (False, True))); 
print_bool (false = eval (OrElse (False, False))); 
print_bool (false = eval (Imply (True, False))); 
print_bool (true = eval (Imply (True, True))); 
print_bool (true = eval (Imply (False, True))); 
print_bool (true = eval (Imply (False, False))); 
print_bool (true = eval (Equal (Num 3, Num 5))); 
print_bool (false = eval (Equal (Num 3, Num 3))); 
print_bool (false = eval (Equal (Num 3, Num 1))); 
print_bool (false = eval 
(Equal (Plus (Num 3, Num 4), Minus (Num 5, Num 1)))); 
print_bool (true = eval 
(Equal (Plus (Num 10, Num 12), Minus (Num 10, Num (-13))))); 


*)


