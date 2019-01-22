
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec has_env : var list -> var -> bool
  = fun li v -> match li with
	[] -> false
	|hd::tl -> if(hd = v) then true else (has_env tl v)

  let rec checklist : exp -> var list -> bool
  = fun exp l -> match exp with
	V(v) -> has_env l v
	|P(v,e) -> let li = v::l in (checklist e li)
	|C(e1,e2) -> (checklist e1 l) && (checklist e2 l)

  let check : exp -> bool
  = fun exp -> let li = [] in checklist exp li (* TODO *)
