(* 8 check: lambda -> bool *)
type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda
and var = string

let check lambda =
	let rec cMetro lambda areas = match lambda with
		V n -> List.mem n areas
		| P (n, m) -> cMetro m (n::areas)
		| C (m0, m1) -> (cMetro m0 areas) && (cMetro m1 areas) in
	
	cMetro lambda []
