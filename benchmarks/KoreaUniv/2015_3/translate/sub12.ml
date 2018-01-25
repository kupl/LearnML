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
  
  type nlenv = var list

  let empty_nlenv = []
  let extend_nlenv x e = x::e
  let rec apply_nlenv_c e x n = match e with
    | [] -> raise (Failure "Environment is empty")
    | hd::tl -> if hd = x then n else (apply_nlenv_c tl x (n+1))
  and apply_nlenv e x = (apply_nlenv_c e x 0)
  
  let rec change : program -> nlenv -> nl_program
  = fun pgm nlenv -> match pgm with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (apply_nlenv nlenv x)
    | ADD (e1,e2) -> 
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      NL_ADD (c1,c2)
    | SUB (e1,e2) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      NL_SUB (c1,c2)
    | ISZERO e ->
      let c = change e nlenv in
      NL_ISZERO c
    | IF (e1,e2,e3) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      let c3 = change e3 nlenv in
      NL_IF (c1,c2,c3)      
    | LET (x,e1,e2) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 (extend_nlenv x nlenv) in
      NL_LET (c1,c2)
    | PROC (x,e) -> 
      let c = change e (extend_nlenv x nlenv) in
      NL_PROC c
    | CALL (e1,e2) ->
      let c1 = change e1 nlenv in
      let c2 = change e2 nlenv in
      NL_CALL (c1,c2)
  
  let translate : program -> nl_program
  =fun pgm -> change pgm empty_nlenv
