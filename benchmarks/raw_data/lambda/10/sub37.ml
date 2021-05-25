(* 4190.310 Programming Language			*
 * Homework #1 - Exercise 7 (CheckMetroMap)	*
 * 2008-11744 Jongwook Choi 				*)

type lambda  = V of var
			| P of var * lambda
			| C of lambda * lambda
	and var = string

let rec check : lambda -> bool = fun m ->
	let rec checkAux env m =
		match m with
			  V var -> List.exists (fun t -> (t = var)) env
			| P (var, m') -> checkAux (var :: env) m'
			| C (m1, m2) -> (checkAux env m1) && (checkAux env m2)
	in
		checkAux [] m
;;

