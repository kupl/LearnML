type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec check varlist lambda =
			match lambda with
			| (V n) -> if (List.mem n varlist) then true else false
			| (P (n, m)) -> (check (List.append [n] varlist) m)
			| (C (m1, m2)) -> (check varlist m1) && (check varlist m2)

let rec check lambda =
			match lambda with
			| (V n) -> false
			| (P (n, m)) -> check [n] m
			| (C (m1, m2)) -> (check m1) && (check m2)
