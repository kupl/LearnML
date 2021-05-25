type lambda = V of var
| P of var * lambda
| C of lambda * lambda

and var = string


let rec check lambda_input = 
	let 
		rec listStation m_input =
			match m_input with
			| V(n) -> [n]
			| P(n,m) -> deleteAll (listStation(m), n)
			| C(m1,m2) -> listStation(m1)@listStation(m2)
	and
		deleteAll (list_input,target) =
			match list_input with
			| l::remain_list -> 
				if l=target then deleteAll(remain_list, target) 
				else l::deleteAll(remain_list, target)
			| [] -> []
	in
		if listStation(lambda_input) = [] then true else false