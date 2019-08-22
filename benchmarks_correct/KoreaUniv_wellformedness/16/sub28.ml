
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check : lambda -> bool
  = fun lambda -> let rec checklst : lambda * var list -> bool
		= fun (ee, llst) -> match ee with
			| V vf ->
			begin
			  match llst with 
				| [] -> false
				| hd :: tl-> if hd = vf then true
										 else checklst ((ee), tl)
			end
			| P (vf, lambdaf) -> checklst (lambdaf, vf::llst)
			| C (lambdaf1, lambdaf2) -> (checklst (lambdaf1, llst)) && (checklst (lambdaf2, llst)) in
		let varlist = [] in
	match lambda with 
	| P (v1, lambda1) -> checklst (lambda1, v1::varlist)
	| _ -> checklst (lambda, varlist);;
