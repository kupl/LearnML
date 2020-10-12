type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec checkProc (met:lambda) (l:var list) : bool =
	match met with
	| V n -> (List.mem n l)
	| P (n,m) -> (checkProc m (l@[n]))
	| C (m1,m2) -> (checkProc m1 l)&&(checkProc m2 l)

let check (met:lambda) : bool =
	match met with
	| V n -> false
	| P (n,m) -> (checkProc m [n])
	| C (m1,m2) -> (checkProc m1 [])&&(checkProc m2 [])
