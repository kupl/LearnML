type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string;;

let check m =
	let rec ismem m lst = 
		match m with
		| V n -> List.mem n lst
		| P (n, m1) -> ismem m1 (n::lst)
		| C (m1, m2) -> ismem m1 lst && ismem m2 lst
	in
	ismem m [];;
