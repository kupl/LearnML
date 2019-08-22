type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string
let check: lambda -> bool =
	let rec cM (met,lst) =
		match met with V n -> (List.mem n lst)
			|P (n,m) -> cM (m, n::lst)
			|C (m1,m2) -> (cM (m1,lst)) && (cM (m2,lst)) in
	fun m -> cM (m,[])
