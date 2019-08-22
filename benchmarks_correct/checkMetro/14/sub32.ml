type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string



let rec checkNameList(l, n) =
	match l with
	| [] -> false
	| hd::tl -> if hd = n then true
				else checkNameList(tl, n)

let rec checkRec (l, m) =
	match m with
	| V n -> checkNameList(l, n)
	| P (n, m0) -> checkRec(n::l, m0)
	| C (m0, m1) -> checkRec(l, m0) && checkRec(l, m1)

let rec check m =
	match m with
	| V n -> false
	| P (n, m0) -> checkRec(n::[], m0)
	| C (m0, m1) -> check m0 && check m1
