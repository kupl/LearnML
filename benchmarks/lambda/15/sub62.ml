type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string;;

let check input =
	let rec aux arealist = function
	| V(m) -> (List.mem m arealist)
	| P(n, m) -> aux (n::arealist) m
	| C(m1, m2) -> (aux arealist m1) && (aux arealist m2) in
	aux [] input;;
