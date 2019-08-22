
type lambda = V of var
	   | P of var * lambda
	   | C of lambda * lambda

and var = string


let check m =

	let rec valify (var_, covers) =
		match covers with V n -> false
		                | P (n, m_) -> ( if n = var_ then
							true
						    else
							valify (var_, m_) )
				| C (m1, m2) -> false 
	in

	let rec findStation (cur, covers) =
		match cur with V n -> valify (n, covers)
			     | P (n, m_) -> findStation (m_, P (n, covers))
			     | C (m1, m2) -> 
				(findStation (m1, covers)) && (findStation (m2, covers))
	in	

	findStation (m, V " ")