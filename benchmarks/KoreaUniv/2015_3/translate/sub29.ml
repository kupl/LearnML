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
  =fun pgm -> NL_CONST 0 (* TODO *)



let rec length l=
    match l with 
      |[]->0
      |hd::tl->1+length tl

let rec check: var list->var->int
=fun lst x->
     match lst with 
        |[]->raise (Failure "free variable")
        |hd::tl->if x=hd then ((length lst)-1) else (check tl x)


let rec makelist lst e=
      match e with
         |CONST n->NL_CONST n 
         |VAR x->NL_VAR (check lst x)
         |ADD (e1,e2)->NL_ADD (makelist lst e1,makelist lst e2)
         |SUB (e1,e2)->NL_SUB (makelist lst e1,makelist lst e2)
         |ISZERO e1->NL_ISZERO (makelist lst e1)
         |IF (e1,e2,e3)->NL_IF (makelist lst e1,makelist lst e2, makelist lst e3)
         |LET(x,e1,e2)->NL_LET(makelist lst e1,makelist (lst@[x]) e2)
         |PROC (x,e1)->NL_PROC(makelist (lst@[x]) e1)
         |CALL (e1,e2)->NL_CALL(makelist lst e1,makelist lst e2)


let rec translate : program -> nl_program
  =fun pgm -> makelist [] pgm
