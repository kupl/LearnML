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

	let rec lexenv : exp -> string list -> string list
	= fun exp l -> match exp with
	|LET (v, e1, e2) -> v::l
	|PROC (v, e) -> v::l
	|_ -> l

	and lookupenv : string -> string list -> int -> int
	= fun v l n-> match l with
	[] -> raise (Failure "Empty environment")
	|x::t -> if x = v then n else lookupenv v t (n+1)

	and convert : program -> string list -> nl_program
	= fun pgm l -> match pgm with
	|CONST n -> NL_CONST n
	|VAR v -> NL_VAR (lookupenv v l 0)
	|ADD (e1, e2) -> NL_ADD(convert e1 l, convert e2 l)
	|SUB (e1, e2) -> NL_SUB(convert e1 l, convert e2 l)
	|ISZERO e1 -> NL_ISZERO (convert e1 l)
	|IF (e1, e2, e3) -> NL_IF(convert e1 l, convert e2 l, convert e3 l)
	|LET (v, e1, e2) -> NL_LET (convert e1 l, convert e2 (lexenv pgm l))
	|PROC (v, e1) -> NL_PROC (convert e1 (lexenv pgm l))
	|CALL (e1, e2) -> NL_CALL(convert e1 l, convert e2 l)

  
  let translate : program -> nl_program
  =fun pgm -> convert pgm [];;
