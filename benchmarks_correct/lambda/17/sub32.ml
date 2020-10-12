type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let check : lambda -> bool = fun met ->
	let rec helper : lambda * string list -> bool = fun (m,l) ->
		match m with
		| V n -> List.mem n l
		| P (n,m) -> helper(m,n::l)
		| C (m1,m2) -> helper(m1,l) && helper(m2,l)

	in
	helper(met,[])
		
