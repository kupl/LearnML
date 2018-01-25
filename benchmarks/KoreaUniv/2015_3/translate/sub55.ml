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
  
let rec helper : program * ('a list) -> nl_program 
= fun (pgm2, l) ->   
  match pgm2 with 
  |CONST n -> NL_CONST n
  |VAR x -> let rec count : ('a list)-> int = fun l -> (match l with
                                              [] -> 0
                                              |h::t -> if x=h then 0 else 1+count(t)
                                            ) in NL_VAR (count l)
  |ADD (e1, e2) -> NL_ADD(helper(e1,l), helper(e2,l))
  |SUB (e1, e2) -> NL_SUB(helper(e1,l), helper(e2,l))
  |ISZERO (e) -> NL_ISZERO(helper(e, l))
  |IF (e1, e2, e3) -> NL_IF(helper(e1, l), helper(e2, l), helper(e3, l))
  |LET (x, e1, e2) -> NL_LET(helper(e1, l), helper(e2, (x::l)))
  |PROC (x, e) -> NL_PROC(helper(e, (x::l)))
  |CALL (e1, e2) -> NL_CALL(helper(e1, l), helper(e2, l))

let translate : program -> nl_program
=fun pgm -> helper(pgm, [])
