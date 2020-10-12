type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string


let rec comparing (var, li) =
	match li with
	| [] -> false
	| st::li' ->
		if(var = st) then true else comparing(var, li')
		
	let rec buff (under, li) =
		match under with
		| V st_var -> comparing(st_var, li)
		| P(id, m) -> buff ( m, List.append li [id])
		| C(m1, m2) -> (buff (m1, li) ) && (buff(m2, li))
		

let rec check under = buff(under, [])
		