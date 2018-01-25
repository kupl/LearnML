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
  
  let translate : program -> nl_program
  =fun pgm -> NL_CONST 0

  let rec index(l,s) =
    match l with
    |[] -> raise (Failure "Environment is empty")
    |h::t -> if h=s then 0 else 1 + index(t,s)

  let rec trans(e,en) =
    match e with
    | CONST n -> NL_CONST n
    | VAR s -> NL_VAR(index(en,s))
    | ADD(v1,v2) -> NL_ADD(trans(v1,en),trans(v2,en))
    | SUB(v1,v2) -> NL_SUB(trans(v1,en),trans(v2,en))
    | LET(s,e1,e2) -> NL_LET(trans(e1,en),trans(e2,[s]@en))
    | PROC(s,e1) -> NL_PROC(trans(e1,[s]@en))
    | ISZERO(e1) -> NL_ISZERO(trans(e1,en))
    | IF(e1,e2,e3) -> NL_IF(trans(e1,en),trans(e2,en),trans(e3,en))
    | CALL(e1,e2) -> NL_CALL(trans(e1,en),trans(e2,en))

  let translate pgm = trans(pgm,[])
