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

type indexBook = var list
let extendIndexBook : var -> indexBook -> indexBook
= fun v iBook -> (v::iBook)


let rec _getIndex : var -> indexBook -> int -> int
= fun v iBook currIndex ->
	match iBook with
	| [] -> raise (Failure "No such variable in Index Book")
	| hd::tl -> if (List.hd iBook) = v then currIndex 
		  else  (_getIndex v tl (currIndex+1))

let getIndex : var -> indexBook -> int
= fun v iBook ->
	_getIndex v iBook 0

let rec _trans : program -> indexBook -> nl_program
= fun pgm iBook ->
	match pgm with
	| CONST (i) -> NL_CONST (i)
	| VAR (v) -> NL_VAR (getIndex v iBook)
	| ADD (e1, e2) -> NL_ADD((_trans e1 iBook), (_trans e2 iBook))
	| SUB (e1, e2) -> NL_SUB((_trans e1 iBook), (_trans e2 iBook))
	| ISZERO (e) -> NL_ISZERO (_trans e iBook)
	| IF (e1, e2, e3) -> NL_IF ((_trans e1 iBook), (_trans e2 iBook), (_trans e3 iBook)) 
	| LET (v, e1, e2) -> NL_LET( (_trans e1 iBook), (_trans e2 (extendIndexBook v iBook)))
	| PROC (v, e) -> NL_PROC (_trans e (extendIndexBook v iBook))
	| CALL (e1, e2) -> NL_CALL ((_trans e1 iBook), (_trans e2 iBook))

let translate : program -> nl_program
=fun pgm -> 
	_trans pgm []
