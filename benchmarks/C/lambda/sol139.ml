type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string
(*
 * manually edited
 * checkMetro -> check 
 * check -> check_sub
 *)
let rec check_sub varlist lambda =
			match lambda with
			| (V n) -> if (List.mem n varlist) then true else false
			| (P (n, m)) -> (check_sub (List.append [n] varlist) m)
			| (C (m1, m2)) -> (check_sub varlist m1) && (check_sub varlist m2)

let rec check lambda =
			match lambda with
			| (V n) -> false
			| (P (n, m)) -> check_sub [n] m
			| (C (m1, m2)) -> (check m1) && (check m2)
