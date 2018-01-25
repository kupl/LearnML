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

  type nl_value = NL_Int of int 
                | NL_Bool of bool 
                | NL_Procedure of nl_exp * nl_env
  and nl_env = nl_value list

let rec nth l n=
match l with
|[]-> raise (Failure "Empty Environment")
|h::t-> if n=0 then h else (nth t (n-1))

let rec find l a=
match l with
|[]-> raise (Failure "Not Found")
|h::t-> if h=a then 0 else (1+(find t a))

let rec trans : program -> 'a list -> nl_program
=fun pgm l ->
match pgm with
| CONST i-> NL_CONST i
| VAR x-> NL_VAR (find l x)
| ADD (a,b)-> NL_ADD(trans a l, trans b l)
| SUB (a,b)-> NL_SUB(trans a l, trans b l)
| ISZERO a-> NL_ISZERO (trans a l)
| IF (a,b,c)-> NL_IF ((trans a l),(trans b l),(trans c l))
| LET (x,a,b)-> NL_LET(trans a l, trans b (x::l))
| PROC (x,a)-> NL_PROC(trans a (x::l))
| CALL (a,b)-> NL_CALL((trans a l), (trans b l))


  let translate : program -> nl_program
  =fun pgm -> trans pgm []
