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

  let varList = []

  let rec popVar : var -> var list -> int
    =fun v l ->
      match l with
          [] -> raise (Failure "empty stack")
        | hd::tl -> if v = hd then 0 else (popVar v tl) + 1

  let rec _trans : exp -> var list -> nl_program
    =fun e l ->
      match e with
          CONST n -> NL_CONST n
        | VAR x -> NL_VAR (popVar x l)
        | ADD (e1, e2) -> NL_ADD (_trans e1 l, _trans e2 l)
        | SUB (e1, e2) -> NL_SUB (_trans e1 l, _trans e2 l)
        | ISZERO e -> NL_ISZERO (_trans e l)
        | IF (e1, e2, e3) -> NL_IF (_trans e1 l, _trans e2 l, _trans e3 l)
        | LET (x, e1, e2) -> NL_LET(_trans e1 l, _trans e2 (x::l))
        | PROC (x, e) -> NL_PROC (_trans e (x::l))
        | CALL (e1, e2) -> NL_CALL (_trans e1 l, _trans e2 l)

  let rec translate : program -> nl_program
    =fun pgm -> _trans pgm varList
