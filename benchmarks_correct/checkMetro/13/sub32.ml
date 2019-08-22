type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec check met = 
	let rec subcheck m areas = 
		match m with 
			V(var) -> (List.mem var areas)
			| P(var, met1) -> (subcheck met1 (var::areas))
			| C(met1, met2) -> (subcheck met1 areas) && (subcheck met2 areas)
	in (subcheck met [])


