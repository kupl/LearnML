type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec in_list el lst =
	match lst with [] -> false
	| head::tail -> (head = el) || in_list el tail

let rec check_sub lambda lst =
        match lambda with
	V s_var -> in_list s_var lst
	| P (a_var, lambda_sub) -> check_sub lambda_sub (a_var::lst)
       	| C (a,b) -> check_sub a lst && check_sub b lst

let check lambda =
	check_sub lambda []

