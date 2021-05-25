type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check lambda = 
	let rec cmModule lambda2 stations =
		match lambda2 with
		| V var2 -> List.mem var2 stations
		| P (var2, lambda3) -> cmModule lambda3 (var2::stations)
		| C (lambda3, lambda4) -> (cmModule lambda3 stations) && (cmModule lambda4 stations)
		in
	cmModule lambda [];;
