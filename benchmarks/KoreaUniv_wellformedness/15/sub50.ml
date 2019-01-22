	type exp = V of var
			 | P of var * exp
			 | C of exp * exp
	and var = string
	
	let check : exp -> bool
	=fun e ->
		let rec check_help m n_list = 
			match m with
			| V n -> List.mem n n_list
			| P (n, e1) -> check_help e1 (n::n_list)
			| C (e1,e2) -> check_help e1 n_list && check_help e2 n_list
		in check_help e []
