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

  let rec lc
  =fun l v index ->
  match l with
  | [] -> raise (Failure "not!")
  | hd::tl -> if (hd=v) then index else lc tl v index+1

  let rec lex : exp -> string list -> nl_exp
  =fun exp envl ->
  	match exp with
	| CONST n -> NL_CONST n
	| VAR x -> NL_VAR (lc envl x 0)
	| ADD (e1,e2) ->
		let v1 = lex e1 envl in
		let v2 = lex e2 envl in
			NL_ADD(v1,v2)
	| SUB (e1,e2) ->
		let v1 = lex e1 envl in
		let v2 = lex e2 envl in
		NL_SUB(v1,v2)
	| ISZERO e -> NL_ISZERO(lex e envl)
	| IF (e1,e2,e3) -> NL_IF(lex e1 envl,lex e2 envl,lex e3 envl)
	| LET (x,e1,e2) -> NL_LET(lex e1 envl, lex e2 (x::envl))
	| PROC (x,e) -> NL_PROC (lex e (x::envl))
	| CALL (e1,e2) -> NL_CALL (lex e1 envl, lex e2 envl)

  let translate : program -> nl_program
  =fun pgm -> lex pgm []
