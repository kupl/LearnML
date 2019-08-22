
  type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
  and var = string

	let check : lambda -> bool
	=fun lambda ->
		let rec check_help m n_list = 
		match m with
		| V n -> List.mem n n_list
		| P (n, m1) -> check_help m1 (n::n_list)
		| C (m1,m2) -> check_help m1 n_list && check_help m2 n_list
	in check_help lambda []
