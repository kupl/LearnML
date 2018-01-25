type program = exp 
and exp = 
	| CONST of int 
	| VAR of var 
	| ADD of exp * exp 
	| SUB of exp * exp 
	| ISZERO of exp 
	| IF of exp * exp * exp 
	| LET of var * exp * exp 
	| PROC of var * exp 
	| CALL of exp * exp 

and var = string 

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

let rec count a =
	match a with
	[] -> 0
	| hd::tl -> 1 + count tl

let rec find a b =
	match a with
	[] -> 0
	| hd::tl -> if hd = b then (count a)-1 else find tl b


let rec evol p ev =
	match p with
	CONST a -> NL_CONST a
	| VAR a -> NL_VAR (find ev a)
	| ADD (a,b) -> NL_ADD (evol a ev , evol b ev )
	| SUB (a,b) -> NL_SUB (evol a ev , evol b ev )
	| ISZERO a -> NL_ISZERO (evol a ev )
	| IF (a,b,c) -> NL_IF (evol a ev , evol b ev , evol c ev )
	| LET (a,b,c) -> NL_LET(evol b (ev) , evol c (ev@[a]) )
	| PROC (a,b) -> NL_PROC(evol b (ev@[a]))
	| CALL (a,b) -> NL_CALL(evol a ev , evol b ev )

let translate a = evol a []
