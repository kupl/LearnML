type lambda =
	| V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check_with_list m l =
	match m with
	| V str -> (List.mem str l)
	| C (a, b) -> (check_with_list a l) && (check_with_list b l)
	| P (str, met) -> check_with_list met (List.append l [str])

let rec check m = check_with_list m []