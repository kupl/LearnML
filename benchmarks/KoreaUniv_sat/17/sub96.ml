(*problem 3*)
type formula =
	True
	|False
	|Var of string
	|Neg of formula
	|And of formula * formula
	|Or of formula * formula
	|Imply of formula * formula
	|Iff of formula * formula
(*
type env = (var * formula) list
let empty_env = []
let extend_env (x,v) e = (x,v)::e
let rec apply_env x e =
	match e with
	|[] -> raise (Failure ("variable" ^ x ^ " not found"))
	|(y,v)::tl -> if x = y then v else apply_env x tl

let rec eval : exp -> env -> bool
= fun exp env ->
	match exp with
	|True -> true
	|False -> false
	|Var x -> if x = true then true else false
	|Neg x -> if x = true then false else true
	|And (e1,e2) -> if sat e1 = sat e2 then true else false
	|Or (e1,e2) ->  if sat e1 = sat e2 then false else true
	|Imply (e1,e2) ->  if sat e1 = false && sat e2 = false then true else false
	|Iff (e1,e2) ->  Or (e1,e2)

let tc list = [(true,true);(true,false);(false,true);(false,false)]
*)
let rec sat : formula -> bool
= fun f -> match f with
			|True -> true
			|False -> false
			|Var x -> true
			|Neg x -> true
			|And (e1,e2) -> if sat e1 = sat e2 then true else false
			|Or (e1,e2) -> if sat e1 = sat e2 then false else true 
			|Imply (e1,e2) -> if sat e1 = true && sat e2 = false then true else false
			|Iff (e1,e2) -> if sat e1 = sat e2 then false else true 
