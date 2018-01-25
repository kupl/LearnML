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

  let rec work : var * var list * int -> nl_exp
  =fun (v,li,n) -> match li with
  |[] -> raise (Failure "Environment Search Error")
  |h::t -> if v=h then NL_VAR n else work(v,t,n+1)

  let find : var * var list -> nl_exp
  =fun (v,li) -> work (v,li,0)

  let rec tt : program -> var list -> nl_program
  =fun pgm li -> match pgm with
  CONST i -> NL_CONST i
| VAR i -> find (i,li)
| ADD (e1,e2) -> NL_ADD(tt e1 li,tt e2 li)
| SUB (e1,e2) -> NL_SUB(tt e1 li,tt e2 li)
| ISZERO e -> NL_ISZERO (tt e li)
| IF (e1,e2,e3) -> NL_IF(tt e1 li,tt e2 li,tt e3 li)
| LET(v,e1,e2) -> let li'=[v]@li in NL_LET(tt e1 li,tt e2 li')
| PROC(v,e) -> let li'=[v]@li in NL_PROC(tt e li')
| CALL(e1,e2) -> NL_CALL(tt e1 li ,tt e2 li)
  

  let translate : program -> nl_program
=fun pgm -> tt pgm []
