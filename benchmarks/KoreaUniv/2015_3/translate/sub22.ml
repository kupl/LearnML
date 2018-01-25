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

  let rec cal_scope lst v = match lst with
  | hd::tl -> if hd=v then 0 else (cal_scope tl v)+1

  let rec nl_translate exp lst = match exp with
  | CONST i -> NL_CONST i
  | VAR v -> NL_VAR (cal_scope lst v)
  | ADD (e1,e2) -> NL_ADD (nl_translate e1 lst, nl_translate e2 lst)
  | SUB (e1,e2) -> NL_SUB (nl_translate e1 lst, nl_translate e2 lst)
  | ISZERO e -> NL_ISZERO (nl_translate e lst)
  | IF (e1,e2,e3) -> NL_IF(nl_translate e1 lst, nl_translate e2 lst, nl_translate e3 lst)
  | LET (v,e1,e2) -> let expand_list = v::lst in NL_LET (nl_translate e1 lst, nl_translate e2 expand_list)
  | PROC (v,e) -> let expand_list = v::lst in NL_PROC (nl_translate e expand_list)
  | CALL (e1,e2) -> NL_CALL (nl_translate e1 lst, nl_translate e2 lst)
   
  let translate : program -> nl_program
  =fun pgm -> nl_translate pgm []
