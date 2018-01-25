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

	let rec find_scope : string * string list * int -> int
	= fun (x,l,n) ->
		match l with
		[] -> 0
	|	h::t -> if x=h then n else find_scope (x,t,n+1)

	let rec translate2 : program * string list -> nl_program
	= fun (pgm,l) ->
		match pgm with
		CONST n -> NL_CONST n
	| VAR x -> NL_VAR (find_scope (x,l,0))
	| ADD (e1,e2) -> NL_ADD (translate2 (e1,l),translate2 (e2,l))
	| SUB (e1,e2) -> NL_SUB (translate2 (e1,l),translate2 (e2,l))
	| ISZERO e -> NL_ISZERO (translate2 (e,l))
	| IF (e1,e2,e3) -> NL_IF (translate2 (e1,l),translate2 (e2,l), translate2 (e3,l))
	| LET (x,e1,e2) -> NL_LET (translate2 (e1,l),translate2 (e2,x::l))
	| PROC (x,e) -> NL_PROC (translate2 (e,x::l))
	| CALL (e1,e2) -> NL_CALL (translate2 (e1,l),translate2 (e2,l))

	let rec translate : program -> nl_program
	= fun pgm -> translate2 (pgm,[])
