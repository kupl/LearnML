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

let rec exist_v: var * string list -> int
= fun (v, l) ->
match l with
[] -> 0
|hd::tl -> if v = hd then 1 else exist_v(v, tl)

(*check if new var is in the var list, if not add to the existing list*)
let rec newlist: string list * string list -> string list
= fun (l1, l2) ->
match l1 with
[] -> l2
|hd::tl -> if exist_v(hd, l2) = 0 then let new_l2 = hd::l2 in newlist(tl, new_l2) 
	else newlist(tl, l2)

(*use to check the place of the variable in the env list*)
let rec find: string * string list -> int
= fun (v, lst) -> 
match lst with 
[] -> 0
|hd::tl -> if hd = v then 0 else 1 + (find (v, tl))

let rec eval: program * string list -> nl_program
= fun (exp, lst) ->
match exp with 
CONST n -> NL_CONST n
|VAR v -> NL_VAR (find (v, lst))
|ADD(e1, e2) -> NL_ADD(eval (e1, lst), eval (e2, lst))
|SUB(e1, e2) -> NL_SUB(eval (e1, lst), eval (e2, lst))
|ISZERO e1 -> NL_ISZERO(eval (e1, lst))
|IF (e1, e2, e3) -> NL_IF(eval (e1, lst), eval(e2, lst), eval (e3, lst))
|LET (v, e1, e2) -> NL_LET (eval (e1, lst), eval (e2, newlist([v], lst)))
|PROC(v, e1) -> NL_PROC (eval(e1, newlist([v], lst)))
|CALL(e1, e2) -> NL_CALL (eval(e1, lst), eval(e2, lst))

let translate : program -> nl_program
=fun p -> eval (p, []) 
