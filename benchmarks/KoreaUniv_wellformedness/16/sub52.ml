
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
  = fun exp -> (*raise NotImplemented*) (* TODO *)
		let varlst = [] in
		let rec checklst : var list -> var -> bool
		= fun lst var ->
			match lst with
				[] -> false
			|	n::lst2 -> if n = var then true else checklst lst2 var
		in
		let rec checkvar : exp -> var list -> bool
		= fun exp lst ->	
			match exp with
			V(v) -> checklst lst v
		|	P(v,e) -> checkvar e (v::lst)
		| C(e1,e2) -> checkvar e1 lst && checkvar e2 lst
		in
		checkvar exp varlst	
				
