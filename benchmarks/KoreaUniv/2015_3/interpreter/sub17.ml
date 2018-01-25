type program = exp 
and exp = 
	| CONST of int 
	| VAR of var 
	| ADD of exp * exp 
	| SUB of exp * exp 
	| ISZERO of exp 
	| IF of exp * exp * exp 
	| LET of var * exp * exp 
	| LETREC of var * var * exp * exp 
	| PROC of var * exp 
	| CALL of exp * exp 
and var = string 

type value = 
	Int of int 
	| Bool of bool 
	| Procedure of var * exp * env 
	| RecProcedure of var * var * exp * env 
and env = var -> value 

let empty_env = fun _ -> raise (Failure "Environment is empty") 

let extend_env (x,v) e = fun y -> if x = y then v else (e y) 

let apply_env e x = e x 

 let cal p =
	match p with
	Int a -> a

let rec eval p ev = 
	match p with
	CONST a -> Int a
	| VAR a -> apply_env ev a
	| ADD(a,b) -> Int (cal (eval a ev) + cal (eval b ev))
	| SUB(a,b) -> Int (cal (eval a ev) - cal (eval b ev))
	| ISZERO a -> if (eval a ev) = Int 0 then Bool true else Bool false
	| IF (a,b,c) -> if (eval a ev) = Bool true then (eval b ev) else (eval c ev)
	| LET (a,b,c) -> eval c (extend_env (a , eval b ev) ev)
	| LETREC(a,b,c,d) -> eval d (extend_env (a, RecProcedure(a,b,c,ev)) ev)
	| PROC (a,b) -> Procedure(a,b,ev)
	| CALL (a1,b1) -> match (eval a1 ev) with
	Procedure(a,b,c) -> eval (LET(a,b1,b)) ev
	| RecProcedure(a,b,c,d) -> eval (LET(b,b1,c)) ev

let run a = eval a empty_env
