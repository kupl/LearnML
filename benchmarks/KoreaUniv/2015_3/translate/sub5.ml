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
  
let rec transform: exp -> var list -> nl_exp 
= fun e env -> 
match e with
| CONST n -> NL_CONST n
| VAR x -> NL_VAR (find x env)
| ADD (e1,e2) -> NL_ADD((transform e1 env), (transform e2 env))
| SUB (e1,e2) -> NL_SUB((transform e1 env), (transform e2 env))
| ISZERO e -> NL_ISZERO(transform e env)
| IF(e1,e2,e3) -> NL_IF((transform e1 env), (transform e2 env), (transform e3 env))
| LET(x,e1,e2) -> NL_LET((transform e1 env), (transform e2 (x::env)))
| PROC(x,e) -> NL_PROC(transform e (x::env))
| CALL(e1,e2) -> NL_CALL((transform e1 env), (transform e2 env))


and find : var -> var list -> int
= fun x l ->
match l with
| hd::tl -> if hd = x then 0
	    else ((find x tl)+1)
| _ -> raise (Failure "error: can not find matching variable")


  let translate : program -> nl_program
  =fun pgm -> transform pgm []
