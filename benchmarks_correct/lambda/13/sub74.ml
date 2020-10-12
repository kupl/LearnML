
type lambda = V of var
						| P of var * lambda
						| C of lambda * lambda
and var = string

let check : lambda -> bool = fun lambda -> 
	let rec checkInner lambda lambdaList = 
		match lambda with
		| V(var) ->	
			List.mem var lambdaList
		| P(var, m) -> 
			checkInner m (var::lambdaList)
		| C(m1, m2) -> 
			(checkInner m1 lambdaList)&&(checkInner m2 lambdaList)
	in 
	
	checkInner lambda []
