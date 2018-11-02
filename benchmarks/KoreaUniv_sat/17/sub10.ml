(* problem 3*)
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let rec apply_env x e =
	match e with
	|[] -> raise (Failure "no value")
	|(y,v)::tl -> if x = y then v else apply_env x tl

let rec check_varlist x vl =
	match vl with
	|[] -> false
	|y::tl -> if x = y then true else check_varlist x tl

let rec eval_f: formula -> (string * bool) list -> bool
= fun f env ->
	match f with
	| True -> true
	| False -> false
	| Var s -> apply_env s env
	| Neg p -> not (eval_f p env)
	| And (p,q) -> (eval_f p env) && (eval_f q env)
	| Or (p,q) -> (eval_f p env) || (eval_f q env)
	| Imply (p,q) -> (not (eval_f p env)) || (eval_f q env)
	| Iff (p,q) -> ((eval_f p env) && (eval_f q env)) || ((not (eval_f p env)) && (not (eval_f q env)))

let rec find_var: formula -> string list -> string list
= fun f vl ->
	match f with
	| True -> vl
	| False -> vl
	| Var s -> if (check_varlist s vl) then vl else s::vl  
	| Neg p -> find_var p vl
	| And (p,q) | Or (p,q) | Imply (p,q) | Iff (p,q) -> find_var p (find_var q vl)

let rec sat_op: formula -> string list -> (string * bool) list -> bool
= fun f varlist env ->
	match varlist with
	|[] -> eval_f f env
	|hd::tl -> let tenv = (hd, true)::env in
			   let fenv = (hd, false)::env in
			   		if (sat_op f tl tenv) then true 
			   		else (sat_op f tl fenv) 

let rec sat : formula -> bool
= fun f ->
	let varlist = find_var f [] in
	let env = [] in
		(sat_op f varlist env)