type lambda = V of var
			|P of var * lambda
			|C of lambda * lambda
and var = string 

let rec contain (l, str) =
	match l with
		| [] -> false
		| h::t -> (h = str) || contain (t, str)

let rec check2 (s, m) =
	match m with 
		| V x -> contain (s, x)
		| C (m1, m2) -> (check2 (s, m1)) && (check2 (s, m2))
		| P (a, b) -> check2 (a::s, b)

let check m = check2 ([], m)	