  type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
  and var = string
  
  let rec is_well_formed : lambda -> var list -> bool
  =fun e env -> 
	match e with
		| V v -> if (List.mem v env) then true else false
		| P (v,lambda') -> let env' = v::env in (is_well_formed lambda' env')
		| C (lambda1, lambda2) -> 
			let r1 = is_well_formed lambda1 env in
			let r2 = is_well_formed lambda2 env in
				r1 && r2

  let check : lambda -> bool
  =fun e -> is_well_formed e [] 
