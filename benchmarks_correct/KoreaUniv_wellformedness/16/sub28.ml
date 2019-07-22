
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp -> let rec checklst : exp * var list -> bool
		= fun (ee, llst) -> match ee with
			| V vf ->
			begin
			  match llst with 
				| [] -> false
				| hd :: tl-> if hd = vf then true
										 else checklst ((ee), tl)
			end
			| P (vf, expf) -> checklst (expf, vf::llst)
			| C (expf1, expf2) -> (checklst (expf1, llst)) && (checklst (expf2, llst)) in
		let varlist = [] in
	match exp with 
	| P (v1, exp1) -> checklst (exp1, v1::varlist)
	| _ -> checklst (exp, varlist);;
