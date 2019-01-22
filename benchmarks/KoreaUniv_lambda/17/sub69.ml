(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec check_occurence v l
= match l with
	| hd::tl -> if (hd = v) then true else check_occurence v tl
	| [] -> false

let rec check_form  : lambda -> var list -> bool
= fun lam l -> match lam with
	| P (v, lambda1) -> check_form lambda1 ([v]@l)
	| V v -> check_occurence v l
	| C (lambda1, lambda2) -> (check_form lambda1 l) && (check_form lambda2 l)
	| _ -> false

let rec check : lambda -> bool
= fun lam -> check_form lam []


