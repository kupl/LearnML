type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let ncor a b = a <> b

let rec slist l = match l with
	| V s -> [s]
	| P (n, m) -> List.filter (fun x -> n <> x) (slist m)
	| C (m1, m2) -> List.append (slist m1) (slist m2)
let rec check lambda = match (slist lambda) with
	| [] -> true
	| _ -> false
