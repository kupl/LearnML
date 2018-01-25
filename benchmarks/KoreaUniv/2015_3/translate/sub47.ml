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
  
	let save : var list -> exp -> var list
	= fun l e -> match e with
	VAR x -> x::l
	|_ -> raise(Failure "Error") 
	
	let rec find : var -> var list -> int
	= fun x l -> match l with
		|[] -> 0
		|h::t -> if x = h then 0 else (find x t) + 1

	let rec help : exp -> var list -> nl_exp
	= fun e l -> match e with
		|CONST n -> NL_CONST n
		|VAR x -> NL_VAR (find x l)
		|ADD(e1,e2) -> NL_ADD(help e1 l, help e2 l)
		|SUB(e1,e2) -> NL_SUB(help e1 l, help e2 l)
		|ISZERO e -> NL_ISZERO(help e l)
		|IF(e1,e2,e3) -> NL_IF(help e1 l, help e2 l , help e3 l)
		|LET(v,e1,e2) -> NL_LET(help e1 l, help e2 (save l (VAR v)))
		|PROC(v,e) -> NL_PROC(help e (save l (VAR v)))
		|CALL(e1,e2) -> NL_CALL(help e1 l, help e2 l)

	
  let rec translate : program -> nl_program
  =fun pgm -> help pgm [] 
