
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

	let rec l_match : string * string list -> bool
	= fun(v,vlist) ->
	match vlist with
	| [] -> false
	| hd::tl -> if hd=v then true else l_match(v,tl)

	let rec v_match : exp * string list -> bool
	= fun(exp,vlist) ->
	match exp with
	| V(v) -> l_match(v,vlist)
	| P(v,e) -> v_match(e,v::vlist)
	| C(e1,e2) -> if v_match(e1,vlist)=true && v_match(e2,vlist)=true then true else false

  let rec check : exp -> bool
  = fun exp -> (* raise NotImplemented  TODO *)
	match exp with
	| V(v) -> false
	| P(v,e) -> v_match(e,[v])
	| C(e1,e2) -> if v_match(e1,[])=true && v_match(e2,[])=true then true else false
