
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

  let rec check : lambda -> bool
  = fun lambda -> (*raise NotImplemented*) (* TODO *)
		let varlst = [] in
		let rec checklst : var list -> var -> bool
		= fun lst var ->
			match lst with
				[] -> false
			|	n::lst2 -> if n = var then true else checklst lst2 var
		in
		let rec checkvar : lambda -> var list -> bool
		= fun lambda lst ->	
			match lambda with
			V(v) -> checklst lst v
		|	P(v,e) -> checkvar e (v::lst)
		| C(e1,e2) -> checkvar e1 lst && checkvar e2 lst
		in
		checkvar lambda varlst	
				
