type lambda = 
V of var 
| P of var * lambda
| C of lambda * lambda
and var = string


let rec check2 (lambda, alist) =
	match lambda with
		V id -> List.mem id alist
		| P (id, m) -> check2 (m, id::alist)
		| C (m1, m2) -> (check2 (m1, alist))&&(check2 (m2, alist));;

let check lambda = check2 (lambda, []);;
