(*2009-11718 1-7*)

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec checkStation (a, lst) =
	match lst with
	[] -> []
	| hd::tl -> (if a=hd then checkStation(a, tl)
		else hd::checkStation(a, tl))

	
let rec check met =
	let rec makeMetro met lst =
		match met with
		V a -> a::lst
		| P (a, mtro) -> checkStation(a, (makeMetro mtro lst))
		| C (met1, met2) -> (makeMetro met1 lst)@(makeMetro met2 lst) in
	if (makeMetro met [])=[] then
	true
	else false

