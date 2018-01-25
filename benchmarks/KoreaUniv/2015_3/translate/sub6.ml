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
  
  let rec index : var -> var list -> int =
    fun x l -> 
    match l with
    []->raise (Failure "'var' must be declare before use")
    |v::tl->if v = x then 0 else 1+ index x tl

  let rec trans : exp->var list ->nl_exp=
    fun exp l ->
    match exp with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (index x l)
    | ADD (e1,e2)-> NL_ADD((trans e1 l),(trans e2 l))
    | SUB (e1,e2)-> NL_SUB((trans e1 l),(trans e2 l))
    | ISZERO e1-> NL_ISZERO(trans e1 l)
    | IF (e1,e2,e3)-> NL_IF((trans e1 l),(trans e2 l),(trans e3 l))
    | LET (x,e1,e2)-> NL_LET((trans e1 l),(trans e2 (x::l)))
    | PROC (x,e1)-> NL_PROC(trans e1 (x::l))
    | CALL (e1,e2)-> NL_CALL((trans e1 l),(trans e2 l))

  let translate : program -> nl_program
  =fun pgm -> trans pgm []
