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
  
  let rec rank elist x =
	match elist with
	 | [] -> 0  
	 | hd::tl -> if hd = x then 0
				else 1 + rank tl x
 
	type elist = nl_exp list
  
  let rec translatewe pgm elist = 
	match pgm with
	 | CONST n -> NL_CONST n
	 | VAR x -> NL_VAR (rank elist x) 
	 | ADD (e1,e2) -> NL_ADD ((translatewe e1 elist) , (translatewe e2 elist))
	 | SUB (e1,e2) -> NL_SUB ((translatewe e1 elist), (translatewe e2 elist))
	 | ISZERO e -> NL_ISZERO (translatewe e elist)
	 | IF (e1,e2,e3) -> NL_IF ((translatewe e1 elist), (translatewe e2 elist), (translatewe e3 elist))
	 | LET (x,e1,e2) -> NL_LET ((translatewe e1 elist), (translatewe e2 ([x]@elist)))
	 | PROC (x,e) -> NL_PROC (translatewe e ([x]@elist))
	 | CALL (e1,e2) -> NL_CALL ((translatewe e1 elist), (translatewe e2 elist))

  
  let rec translate : program -> nl_program
  =fun pgm -> translatewe pgm []

