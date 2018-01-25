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
 
  let rec find : string list -> string -> int -> int
  =fun l x start -> match l with
    | [] -> raise (Failure "Error: Not found")
    | hd::tl -> if hd=x then start else find tl x (start+1)

  let rec translation : program -> string list -> nl_program
  =fun pgm l -> match pgm with
    | CONST n -> NL_CONST n
    | VAR x -> NL_VAR (find l x 0)
    | ADD (e1,e2) -> NL_ADD (translation e1 l, translation e2 l)
    | SUB (e1,e2) -> NL_SUB (translation e1 l, translation e2 l)
    | ISZERO e -> NL_ISZERO (translation e l)
    | IF (e1,e2,e3) -> NL_IF (translation e1 l, translation e2 l, translation e3 l)
    | LET (x,e1,e2) -> NL_LET (translation e1 l, translation e2 (x::l))
    | PROC (x,e) -> NL_PROC (translation e (x::l))
    | CALL (e1,e2) -> NL_CALL (translation e1 l, translation e2 l)

  let translate : program -> nl_program
  =fun pgm -> translation pgm []
