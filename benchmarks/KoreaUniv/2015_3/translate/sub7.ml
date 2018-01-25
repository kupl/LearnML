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
and rep = var list

let empty_rep = []
let rec apply_rep x r =
  match r with
    | [] -> raise (Failure "Representation is empty")
    | y::tl -> if x = y then 0 else 1+apply_rep x tl
let extend_rep x r = x::r

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

let rec convert : exp -> rep -> nl_program
  = fun exp rep ->
    match exp with
      | CONST(n) -> NL_CONST(n)
      | VAR(x) -> NL_VAR(apply_rep x rep)
      | ADD(e1, e2) -> NL_ADD(convert e1 rep, convert e2 rep)
      | SUB(e1, e2) -> NL_SUB(convert e1 rep, convert e2 rep)
      | ISZERO(e) -> NL_ISZERO(convert e rep)
      | IF(e1, e2, e3) -> NL_IF(convert e1 rep, convert e2 rep, convert e3 rep)
      | LET(x, e1, e2) -> 
          let rep2 = extend_rep x rep
          in NL_LET(convert e1 rep, convert e2 rep2)
      | PROC(x, e) ->
          let rep2 = extend_rep x rep
          in NL_PROC(convert e rep2)
      | CALL(e1, e2) ->
          NL_CALL(convert e1 rep, convert e2 rep)
;;
let rec translate : program -> nl_program
  = fun pgm -> convert pgm empty_rep
;;