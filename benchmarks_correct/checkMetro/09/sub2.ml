type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
			and var = string

let rec check a = 
	let rec checkm m ls =
		match m with
		V(var) -> List.mem var ls
		|P (n, m) -> checkm m (n::ls)
		|C(m1, m2) -> (checkm m1 ls) && (checkm m2 ls) in
	checkm a []
