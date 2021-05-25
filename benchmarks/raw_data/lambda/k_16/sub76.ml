
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec has_env : var list -> var -> bool
  = fun li v -> match li with
	[] -> false
	|hd::tl -> if(hd = v) then true else (has_env tl v)

  let rec checklist : lambda -> var list -> bool
  = fun lambda l -> match lambda with
	V(v) -> has_env l v
	|P(v,e) -> let li = v::l in (checklist e li)
	|C(e1,e2) -> (checklist e1 l) && (checklist e2 l)

  let check : lambda -> bool
  = fun lambda -> let li = [] in checklist lambda li (* TODO *)
