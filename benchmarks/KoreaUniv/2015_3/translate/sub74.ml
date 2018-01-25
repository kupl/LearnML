type program = exp
and exp =
		|CONST of int
		|VAR of var
		|ADD of exp * exp
		|SUB of exp * exp
		|ISZERO of exp
		|IF of exp * exp * exp
		|LET of var * exp * exp
		|PROC of var * exp
		|CALL of exp * exp
and var = string
and rep = var list

let empty_rep = []
let rec apply_rep x e =
		match e with
		|[]->raise (Failure "Representation is empty")
		|h::tl -> if x=h then 0 else 1+apply_rep x tl
let extend_rep x e= x::e


type nl_program = nl_exp
and nl_exp = 
		|NL_CONST of int
		|NL_VAR of int
		|NL_ADD of nl_exp * nl_exp
		|NL_SUB of nl_exp * nl_exp
		|NL_ISZERO of nl_exp
		|NL_IF of nl_exp * nl_exp * nl_exp
		|NL_LET of nl_exp * nl_exp
		|NL_PROC of nl_exp
		|NL_CALL of nl_exp * nl_exp

let rec chg : exp -> rep -> nl_program
=fun exp rep->
		match exp with
		|CONST (n) -> NL_CONST (n)
		|VAR (x) -> NL_VAR(apply_rep x rep)
		|ADD (e1, e2) -> NL_ADD(chg e1 rep, chg e2 rep)
		|SUB(e1, e2) ->NL_SUB (chg e1 rep, chg e2 rep)
		|ISZERO (e) ->NL_ISZERO(chg e rep)
		|IF(e1, e2, e3) -> NL_IF (chg e1 rep, chg e2 rep, chg e3 rep)
		|LET (x, e1, e2)->
			let reps = extend_rep x rep in NL_LET (chg e1 rep, chg e2 reps)
		|PROC (x,e)->
			let reps = extend_rep x rep in 	NL_PROC(chg e reps)
		|CALL(e1, e2) ->
			NL_CALL(chg e1 rep, chg e2 rep)

let rec translate : program -> nl_program
=fun pgm -> chg pgm empty_rep
;;
