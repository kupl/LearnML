type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string


let rec check_list m a_list =
	match m with
	| V s -> List.mem s a_list
	| P (a , m1) -> check_list m1 (a::a_list)
	| C (m2 , m3) -> (check_list m2 a_list) && (check_list m3 a_list)


let check m = check_list m []

