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

	let rec lecaddr : string list * string -> int
	=fun (l,v) -> match l with
							| hd::tl -> if hd = v then 0
													else 1 + lecaddr (tl,v)
							| _ -> 0

	let rec parse : program * string list -> nl_program
	=fun (pgm, l) -> match pgm with
											| CONST n -> NL_CONST n
											| VAR x -> NL_VAR (lecaddr(l,x))
											| ADD (e1,e2) -> NL_ADD (parse(e1,l), parse(e2,l))
											| SUB (e1,e2) -> NL_SUB (parse(e1,l), parse(e2,l))
											| ISZERO e -> NL_ISZERO (parse(e,l))
											| IF (e1,e2,e3) -> NL_IF (parse(e1,l), parse(e2,l), parse(e3,l))
											| LET (x,e1,e2) -> NL_LET (parse(e1,l), parse(e2,x::l))
											| PROC (x,e) -> NL_PROC (parse(e,(x::l)))
											| CALL (e1,e2) -> NL_CALL (parse(e1,l), parse(e2,l))

  let rec translate : program -> nl_program
  =fun pgm -> parse (pgm,[])
