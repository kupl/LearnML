type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check = (fun x -> 
	let rec icm = (fun x l -> match x with
	| V n -> (List.mem n l)
	| C (a, b) -> (icm a l) && (icm b l)
	| P (n, a) -> (icm a (l@[n]))
		) in (icm x [])
);;