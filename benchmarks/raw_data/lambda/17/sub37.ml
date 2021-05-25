type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check (x : lambda) : bool = 
	let rec checkIter ((x : lambda), (nlist : string list)) : bool = 
		match x with
		| V a -> (List.mem a nlist)
		| P (n, m) -> checkIter (m, n :: nlist)
		| C (a, b) -> (checkIter (a, nlist)) && (checkIter (b, nlist))
	in
	checkIter(x, [])
