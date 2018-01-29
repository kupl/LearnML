
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string


	let rec find : var list * var -> bool
	= fun (lst,v) ->
	 match lst with
	| hd::tl -> if hd=v then true else find(tl,v)
	| _-> false

	let rec f: var list * exp -> bool
	= fun	(lst,e) ->
		match e with
		| V v -> if find(lst, v) then true  else false
		| P (v,e) -> if f(lst@[v],e) then true else false
		| C (e1,e2) -> if check(e1)&&check(e2) then true else false

  and check : exp -> bool
  = fun exp -> (*raise NotImplemented*)
		match exp with
		| V v -> false
		| P (v,e) -> if f([v],e) then true else false
		| C (e1,e2) -> if check(e1)&&check(e2) then true else false
