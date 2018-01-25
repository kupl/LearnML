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
  =fun pgm -> 
    let env_lst = []
    in let rec find_index 
    =fun str lst ->
      match lst with
      | h::t -> if h = str then 0 else (find_index str t)+1
      | _ -> -1 
    in let rec exp_trans  
    =fun pgm_exp l -> 
      match pgm_exp with
      | CONST n -> NL_CONST n 
      | VAR x -> NL_VAR (find_index x l) 
      | ADD (e1,e2) -> NL_ADD (exp_trans e1 l, exp_trans e2 l) 
      | SUB (e1,e2) -> NL_SUB (exp_trans e1 l, exp_trans e2 l) 
      | ISZERO e -> NL_ISZERO (exp_trans e l) 
      | IF (e1,e2,e3) -> NL_IF (exp_trans e1 l, exp_trans e2 l, exp_trans e3 l) 
      | LET (x,e1,e2) -> NL_LET (exp_trans e1 l, exp_trans e2 (x::l)) 
      | PROC (x,e) -> NL_PROC (exp_trans e (x::l))
      | CALL (e1,e2) -> NL_CALL (exp_trans e1 l, exp_trans e2 l) 
    in exp_trans pgm env_lst
