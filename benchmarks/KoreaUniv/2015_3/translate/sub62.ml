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

let translate : program -> nl_program =fun pgm -> NL_CONST 0
 
let rec number: var -> var list-> int -> int 
=fun v lst a-> match lst with
	|[]-> raise(Failure"error")
	| hd::tl-> if (v=hd) then a else (number v tl a+1) 

let rec bft pgm a =  match pgm with
	| CONST i-> NL_CONST i
    | VAR v -> NL_VAR (number v a 0)
    | ADD (e1,e2) -> NL_ADD(bft e1 a, bft e2 a) 
    | SUB (e1,e2) -> NL_SUB( (bft e1 a) , (bft e2 a) )
    | ISZERO e-> NL_ISZERO (bft e a)
    | IF (e1,e2,e3) -> NL_IF((bft e1 a),(bft e2  a),(bft e3  a))
    | LET (var,exp1,exp2) -> NL_LET (bft exp1 a  ,bft exp2 ([var]@a) )
    | PROC (var,exp) -> NL_PROC( bft exp ([var]@a) )
    | CALL (e1,e2) -> NL_CALL( (bft e1 a), (bft e2 a) )

 let translate : program -> nl_program
  =fun pgm -> bft pgm []
 