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

let rec depth v lst =
  match lst with 
  | [] -> raise(Failure "not Bound")
  | hd :: tl -> if hd = v then 0 else 1 + (depth v tl)

let rec trans : program -> var list -> nl_program
  = fun pgm lst ->
    match pgm with 
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (depth x lst)
    | ADD (e1, e2) -> NL_ADD (trans e1 lst, trans e2 lst)
    | SUB (e1, e2) -> NL_SUB (trans e1 lst, trans e2 lst)
    | ISZERO e -> NL_ISZERO (trans e lst)
    | IF (e1, e2, e3) -> NL_IF (trans e1 lst, trans e2 lst, trans e3 lst)
    | LET (x, e1, e2) -> NL_LET (trans e1 (lst), trans e2 (x::lst))
    | PROC (x, e1) -> NL_PROC (trans e1 (x::lst))
    | CALL (e1, e2) -> NL_CALL (trans e1 lst, trans e2 lst)


let rec translate : program -> nl_program
  = fun pgm -> trans pgm []
