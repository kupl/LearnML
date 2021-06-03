type var = string
type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda

let check m =
	let rec checkA (met, l) =  match met with
		V n -> List.exists (fun x -> x = n) l
	  | P(n, m) -> checkA(m, n::l)
	  | C(m1, m2) ->
			if checkA(m1, l) then
				if checkA(m2, l) then true
				else false
			else false
	in
	
	checkA(m, [])