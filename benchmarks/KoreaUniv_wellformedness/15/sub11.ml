  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  
  let rec is_well_formed : exp -> var list -> bool
  =fun e env -> 
	match e with
		| V v -> if (List.mem v env) then true else false
		| P (v,exp') -> let env' = v::env in (is_well_formed exp' env')
		| C (exp1, exp2) -> 
			let r1 = is_well_formed exp1 env in
			let r2 = is_well_formed exp2 env in
				r1 && r2

  let check : exp -> bool
  =fun e -> is_well_formed e [] 
