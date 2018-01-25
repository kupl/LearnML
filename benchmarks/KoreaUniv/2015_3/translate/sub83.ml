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
| CALL of exp * exp and var = string type nl_program = nl_exp 
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

let varList = []

let putVarList varList k = (match varList with
	|[]->[k]
	|hd::tl -> k::hd::tl)
	
let cnt = 0 
let rec countVar varList x cnt= 
		(match varList with
			|[]->0
			|hd::tl -> if hd!=x then countVar tl x cnt+1 else cnt) 

let rec translate : program -> nl_program =fun pgm -> 
	(*let rec translate2 pgm varList cnt = *)
	match pgm with
|CONST x -> NL_CONST x
|VAR x -> NL_CONST (countVar varList x 0)
|ADD(x,y) -> NL_ADD(translate x,translate y)
|SUB(x,y)->NL_SUB(translate x, translate y)
|ISZERO(x)->NL_ISZERO(translate x)
|IF(x,y,z)->NL_IF(translate x,translate y,translate z)
|LET(x,y,z)->NL_LET(translate y,translate z)
|PROC(x,y)->NL_PROC(translate y)
|CALL(x,y)->NL_CALL(translate x, translate y)
(* TODO *) 