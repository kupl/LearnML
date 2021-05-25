type lambda =
	| V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec check (m: lambda): bool = 
	let rec cm_iter (met: lambda) (anl: var list): bool =
		match met with
		| V n -> List.mem n anl
		| P (p, q) -> cm_iter q (p::anl)
		| C (p, q) -> (cm_iter p anl) && (cm_iter q anl)
	in cm_iter m []