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

let rec exp_change : string -> string list-> int -> int
= fun x l n -> match l with
| [] -> raise(Failure "no data")
| hd::tl -> if hd=x then n else exp_change x tl (n+1)

and change : (exp * string list) -> nl_exp
= fun (exp,l) -> match exp with
| CONST n -> NL_CONST n
| VAR x -> NL_VAR(exp_change x l 0)
| ADD(e1,e2) -> NL_ADD(change(e1,l),change(e2,l))
| SUB(e1,e2) -> NL_SUB(change(e1,l),change(e2,l))
| ISZERO e -> NL_ISZERO(change(e,l))
| IF(e1,e2,e3) -> NL_IF(change(e1,l),change(e2,l),change(e3,l))
| LET(x,e1,e2) -> NL_LET(change(e1,l),change(e2,x::l))
| PROC(x,e) -> NL_PROC(change(e,x::l))
| CALL(e1,e2) -> NL_CALL(change(e1,l),change(e2,l))

  let translate : program -> nl_program
  =fun pgm -> change(pgm,[])
