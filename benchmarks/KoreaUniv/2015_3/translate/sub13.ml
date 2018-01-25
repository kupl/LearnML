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
  
  let rec cl : string list * string *int -> int
=fun (lst, x, cnt) ->
  match lst with
  |[] -> raise(Failure "Error")
  |hd::tl -> if hd =  x then cnt else cl(tl, x, cnt+1)

let rec nt : program * string list -> nl_program
=fun (exp, lst) ->
  match exp with
  |CONST n -> NL_CONST n
  |VAR v -> NL_VAR (cl(lst, v, 0))
  |ADD (e1, e2) -> NL_ADD(nt(e1, lst), nt(e2, lst))
  |SUB (e1, e2) -> NL_SUB(nt(e1, lst), nt(e2, lst))
  |ISZERO (e) -> NL_ISZERO(nt(e, lst))
  |IF (e1, e2, e3) -> NL_IF(nt(e1, lst), nt(e2, lst), nt(e3, lst))
  |LET (x, e1, e2) -> NL_LET(nt(e1, lst), nt(e2, x::lst))
  |PROC (x, e) -> NL_PROC(nt(e, x::lst))
  |CALL (e1, e2) -> NL_CALL(nt(e1, lst), nt(e2, lst))

let translate : program -> nl_program
=fun pgm -> nt(pgm, [])
