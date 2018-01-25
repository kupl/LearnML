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
  =fun pgm -> NL_CONST 0 

  let find_index elt lst =
    let rec find_it elt acc = function
    | hd :: tl when elt = hd -> acc 
    | hd :: tl -> find_it elt (acc + 1) tl 
    | _ -> raise Not_found 
  in find_it elt 0 lst 

  let rec trans (pgm,envl)=
     match pgm with
     | CONST(a) -> NL_CONST a
     | VAR(x)-> NL_VAR (find_index x envl) 
     | ADD(e1,e2)-> NL_ADD(trans(e1,envl),trans(e2,envl))
     | SUB(e1,e2)-> NL_SUB(trans(e1,envl),trans(e2,envl))
     | ISZERO(e1)-> NL_ISZERO(trans(e1,envl))
     | IF(e1,e2,e3)-> NL_IF(trans(e1,envl),trans(e2,envl),trans(e3,envl))
     | LET(x,e1,e2)-> NL_LET(trans(e1,envl),trans(e2,[x]@envl))
     | PROC(x,e)-> NL_PROC(trans(e,[x]@envl))
     | CALL(e1,e2)-> NL_CALL(trans(e1,envl),trans(e2,envl))

let translate : program ->nl_program
=fun pgm-> trans (pgm,[])
