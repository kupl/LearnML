type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string


let rec check2 id = function
	| [] -> false
	| hd::tl ->
	  if hd=id then true
	  else check2(id)(tl)

let rec foo areas = function
	| V (id) -> check2(id)(areas)
	| P (id, m) -> foo([id]@areas)(m)
	| C (m1, m2) -> foo(areas)(m1) && foo(areas)(m2)

let rec check = foo []



