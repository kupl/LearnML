type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string

let check lambda =
	let rec is_member e l =
		match l with
		  [] -> false
		| h::t -> (e = h) || is_member e t
	in
	let rec check mtr alst =
		match mtr with
		  V (var) -> is_member var alst
		| P (var, m) -> check m (var::alst)
		| C (m1, m2) -> check m1 alst && check m2 alst
	in
	check lambda []
