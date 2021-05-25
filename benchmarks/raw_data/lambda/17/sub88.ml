type lambda = 
	V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let rec isin : (var * var list) -> bool = fun (id, nlist) ->
	match nlist with
	| [] -> false
	| hd::tl -> if hd = id then true else isin (id, tl)

let rec checkList : (lambda * var list) -> bool = fun (m, nlist) ->
	match m with
	| V id -> isin (id, nlist)
	| P (id, m_) -> checkList (m_, (id::nlist))
	| C (m1, m2) -> (checkList (m1, nlist)) && (checkList (m2, nlist))

let check : lambda -> bool = fun m -> checkList (m, [])
