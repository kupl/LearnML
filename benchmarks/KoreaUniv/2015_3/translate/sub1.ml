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

let rec countDepth v lst=
match lst with 
|[]->raise(Failure "not Bound")
|hd::tl->if hd=v then 0 else 1+countDepth v tl

let rec transform : program-> var list-> nl_program
=fun pgm lst-> match pgm with 
  | CONST n-> NL_CONST n
  | VAR x-> NL_VAR (countDepth x lst)
  | ADD (e1, e2) -> NL_ADD(transform e1 lst, transform e2 lst)
  | SUB (e1, e2) -> NL_SUB(transform e1 lst, transform e2 lst)
  | ISZERO e-> NL_ISZERO(transform e lst)
  | IF (e1, e2, e3) -> NL_IF(transform e1 lst, transform e2 lst, transform e3 lst)
  | LET (x, e1, e2) -> NL_LET(transform e1 (lst), transform e2 (x::lst))
  | PROC (x, e1) -> NL_PROC(transform e1 (x::lst))
  | CALL (e1, e2) -> NL_CALL (transform e1 lst, transform e2 lst)


let rec translate : program -> nl_program
=fun pgm-> transform pgm [] 
