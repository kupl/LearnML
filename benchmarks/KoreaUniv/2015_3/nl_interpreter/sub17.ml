type nl_program = nl_exp 
and nl_exp = 
	| NL_CONST of int 
	| NL_VAR of int 
	| NL_ADD of nl_exp * nl_exp 
	| NL_SUB of nl_exp * nl_exp 
	| NL_ISZERO of nl_exp 
	| NL_IF of nl_exp * nl_exp * nl_exp 
	| NL_LET of nl_exp * nl_exp 
	| NL_PROC of nl_exp 
	| NL_CALL of nl_exp * nl_exp 

type nl_value = 
	NL_Int of int 
	| NL_Bool of bool 
	| NL_Procedure of nl_exp * nl_env 

and nl_env = nl_value list 

let rec count a =
	match a with
	[] -> 0
	| hd::tl -> 1 + count tl

let rec find ev a =
	match ev with
	[] -> raise(Failure"Enviroment is empty")
	| hd::tl -> if (count ev) - a = 1 then hd else
	find tl a

let cal a = match a with
	NL_Int a -> a
	| NL_Bool _ -> 1
	| NL_Procedure _ -> 1

let rec evil p ev =
	match p with
	NL_CONST a -> NL_Int a
	| NL_VAR a -> (find ev a)
	| NL_ADD (a,b) -> NL_Int (cal (evil a ev) + cal (evil b ev))
	| NL_SUB (a,b) -> NL_Int (cal (evil a ev) - cal (evil b ev))
	| NL_ISZERO a -> if (evil a ev) = NL_Int 0 then NL_Bool true else NL_Bool false
	| NL_IF (a,b,c) -> if (evil a ev) = NL_Bool true then (evil b ev) else (evil c ev)
	| NL_LET (a,b) -> evil b (ev@[(evil a ev)])
	| NL_PROC a -> NL_Procedure(a,ev)
	| NL_CALL (a,b) -> match (evil a ev) with
	NL_Procedure(a,b) -> (evil a (ev@[NL_Procedure(a,b)]))

let nl_run a =	evil a []