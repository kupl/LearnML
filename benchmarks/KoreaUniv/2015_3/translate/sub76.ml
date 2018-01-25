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
  
  let rec translate : program -> nl_program
  =fun pgm ->
     let rec num_var : exp -> var -> int -> exp = fun exp v n ->
     match exp with
     | CONST i -> CONST i
     | VAR v' -> if v' = v then VAR (string_of_int n) else VAR v'
     | ADD (x,y) -> ADD (num_var x v n, num_var y v n)
     | SUB (x,y) -> SUB (num_var x v n, num_var y v n)
     | ISZERO x -> ISZERO (num_var x v n)
     | IF (x,y,z) -> IF (num_var x v n, num_var y v n, num_var z v n)
     | LET (v',x,y) -> LET (v', (num_var x v n), (num_var y v (n+1)))
     | PROC (v',x) -> PROC (v', (num_var x v (n+1)))
     | CALL (x,y) -> CALL ((num_var x v (n+1)), (num_var y v (n+1)))
   in

    match pgm with
     | LET (v,x,y) -> NL_LET (translate x, translate (num_var y v 0))
     | PROC (v,x) -> NL_PROC (translate (num_var x v 0))
     | CONST i -> NL_CONST i
     | VAR v -> NL_VAR (int_of_string v)
     | ADD (x,y) -> NL_ADD (translate x, translate y)
     | SUB (x,y) -> NL_SUB (translate x, translate y)
     | ISZERO x -> NL_ISZERO (translate x)
     | IF (x,y,z) -> NL_IF (translate x, translate y , translate z)
     | CALL (x,y) -> NL_CALL (translate x, translate y)
