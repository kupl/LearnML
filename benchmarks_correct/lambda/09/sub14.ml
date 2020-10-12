(* 2007-11651 KIM DONG HYUN *)

type lambda = V of var
						| P of var * lambda
						| C of lambda * lambda
and var = string

let check lambda =
	let rec getStn m =
		match m with
			V var -> [var]
		| P (n, m')
		-> List.filter (fun x->(x!=n)) (getStn m')
		| C (m1, m2) -> (getStn m1) @ (getStn m2)
	in
	
	if (getStn lambda) = [] then true
	else false;;
