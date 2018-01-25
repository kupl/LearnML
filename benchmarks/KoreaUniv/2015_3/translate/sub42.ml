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

  let rec compare v l idx = match l with
    [] -> raise (Failure "ERROR")
    |hd::tl -> if(hd=v) then idx else compare v tl idx+1

  let rec translate2 : exp -> string list -> nl_exp 
  =fun e l -> match e with
  CONST n -> NL_CONST n
  |VAR v -> NL_VAR (compare v l 0)
  |ADD (e1, e2) -> NL_ADD (translate2 e1 l, translate2 e2 l)
  |SUB (e1, e2) -> NL_SUB (translate2 e1 l, translate2 e2 l)
  |ISZERO e -> NL_ISZERO (translate2 e l)
  |IF (e1, e2, e3) -> NL_IF (translate2 e1 l, translate2 e2 l, translate2 e3 l)
  |LET (x, e2, e3) -> let p = x::l in
                              NL_LET(translate2 e2 l, translate2 e3 p)
  |PROC (x, e) -> let p = x::l in
                  NL_PROC (translate2 e p)
  |CALL (e1, e2) -> NL_CALL (translate2 e1 l, translate2 e2 l)

  let translate
  =fun pgm ->translate2 pgm []
