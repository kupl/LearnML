(* EXERCISE 1 *)
type expr = NUM of int
	| PLUS of expr * expr
	| MINUS of expr * expr

let rec expreval : expr -> int = fun input ->
	match input with
	| NUM (n) -> n
	| PLUS (exp1, exp2) -> (expreval exp1) + (expreval exp2)
	| MINUS (exp1, exp2) -> (expreval exp1) - (expreval exp2)

type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr

let rec eval : formula -> bool = fun input ->
	match input with
	| TRUE -> true
	| FALSE -> false
	| NOT (subf) -> not (eval subf)
	| ANDALSO (subf1, subf2) -> (eval subf1) && (eval subf2)
	| ORELSE (subf1, subf2) -> (eval subf1) || (eval subf2)
	| IMPLY (subf1, subf2) -> if (eval subf1) && (not (eval subf2)) then false
				else true
	| LESS (exp1, exp2) -> if (expreval exp1) < (expreval exp2) then true
				else false
(*
let _ = 
let print_bool x = 
print_endline (string_of_bool x) in 
print_bool (true = eval TRUE); 
print_bool (false = eval FALSE); 
print_bool (false = eval (NOT TRUE)); 
print_bool (true = eval (NOT FALSE)); 
print_bool (true = eval (ANDALSO (TRUE, TRUE))); 
print_bool (false = eval (ANDALSO (TRUE, FALSE))); 
print_bool (false = eval (ANDALSO (FALSE, TRUE))); 
print_bool (false = eval (ANDALSO (FALSE, FALSE))); 
print_bool (true = eval (ORELSE (TRUE, TRUE))); 
print_bool (true = eval (ORELSE (TRUE, FALSE))); 
print_bool (true = eval (ORELSE (FALSE, TRUE))); 
print_bool (false = eval (ORELSE (FALSE, FALSE))); 
print_bool (false = eval (IMPLY (TRUE, FALSE))); 
print_bool (true = eval (IMPLY (TRUE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, TRUE))); 
print_bool (true = eval (IMPLY (FALSE, FALSE))); 
print_bool (true = eval (LESS (NUM 3, NUM 5))); 
print_bool (false = eval (LESS (NUM 3, NUM 3))); 
print_bool (false = eval (LESS (NUM 3, NUM 1))); 
print_bool (false = eval 
(LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1)))); 
print_bool (true = eval 
(LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13))))); 


*)


