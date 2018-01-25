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

  let rec c : string list * string * int -> int
	= fun (lst, x, cnt) ->
		match lst with
		|[] -> raise(Failure "error!!")
		|hd::tl -> if hd = x then cnt else c(tl, x, cnt+1)
  
  let rec n : program * string list -> nl_program
	= fun (exp, lst) ->
		match exp with
		|CONST n -> NL_CONST n
		|VAR v -> NL_VAR (c(lst, v, 0))
		|ADD (exp1, exp2) -> NL_ADD(n(exp1, lst), n(exp2, lst))
		|SUB (exp1, exp2) -> NL_SUB(n(exp1, lst), n(exp2, lst))
		|ISZERO (exp) -> NL_ISZERO(n(exp, lst))
		|IF (exp1, exp2, exp3) -> NL_IF(n(exp1, lst), n(exp2, lst), n(exp3, lst))
		|LET(x, exp1, exp2) -> NL_LET(n(exp1, lst), n(exp2, x::lst))
		|PROC (x, exp) -> NL_PROC(n(exp, x::lst))
		|CALL (exp1, exp2) -> NL_CALL(n(exp1, lst), n(exp2, lst))
  
  let translate : program -> nl_program
  =fun pgm -> n(pgm, [])
