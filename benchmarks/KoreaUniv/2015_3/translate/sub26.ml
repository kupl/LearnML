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

let rec checkstack : string list * string * int -> int
= fun (lst,v,cnt) ->
match lst with
[] -> raise (Failure "Environment is empty")
| h::t -> if(v=h) then cnt else checkstack(t,v,cnt+1)

let rec nl_translate : program * string list -> nl_program
= fun (pgm,lst) ->
match pgm with
CONST a -> NL_CONST a
| VAR v -> NL_VAR (checkstack(lst,v,0))
| ADD (ex1,ex2) -> NL_ADD(nl_translate(ex1,lst),nl_translate(ex2,lst))
| SUB (ex1,ex2) -> NL_SUB(nl_translate(ex1,lst),nl_translate(ex2,lst))
| ISZERO (ex) -> NL_ISZERO(nl_translate(ex,lst))
| IF (ex1,ex2,ex3) -> NL_IF(nl_translate(ex1,lst),nl_translate(ex2,lst),nl_translate(ex3,lst))
| LET (v,ex1,ex2) -> NL_LET(nl_translate(ex1,lst),nl_translate(ex2,v::lst))
| PROC (v,ex) -> NL_PROC(nl_translate(ex,v::lst))
| CALL (ex1,ex2) -> NL_CALL(nl_translate(ex1,lst),nl_translate(ex2,lst))

let translate : program -> nl_program
=fun pgm ->  (* TODO *)
nl_translate(pgm,[])
