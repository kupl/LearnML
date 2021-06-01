(* not tested *)

type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda

and var = string

let rec lst_check (l: var list) (elt: var) = 
	match l with
	| hd::tl -> if (hd = elt) then true
				else (lst_check tl elt)
	| [] -> false

let check (m: lambda) = 
	let rec check_sub (m: lambda) (sl: var list) = 
		match m with
		| V n -> lst_check sl n
		| P (n, sub_lambda) -> (check_sub sub_lambda (sl@[n]))
		| C (m1, m2) -> (check_sub m1 sl) && (check_sub m2 sl)
	in
	check_sub m []
