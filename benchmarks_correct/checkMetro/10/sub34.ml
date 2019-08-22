type lambda = V of var | P of var * lambda | C of lambda * lambda
and var = string

let check m =
	let rec checkRec avail m =
		match m with
			V s -> List.mem s avail
			| P (n,m1) -> checkRec (n::avail) m1
			| C (m1,m2) -> (checkRec avail m1) && (checkRec avail m2)
	in
		checkRec [] m