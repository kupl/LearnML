type lambda = 
V of var 
| P of var * lambda
| C of lambda * lambda
and var = string


let rec check (lambda, alist) =
	match lambda with
		V id -> List.mem id alist
		| P (id, m) -> check (m, id::alist)
		| C (m1, m2) -> (check (m1, alist))&&(check (m2, alist));;

let check lambda = check (lambda, []);;
