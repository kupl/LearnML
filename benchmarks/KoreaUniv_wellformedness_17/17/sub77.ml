(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let extend v lis = v::lis

let rec find v lis = match lis with
					| [] -> false
					| hd::tl -> if hd = v then true else find v tl

let rec checkfree : lambda -> var list -> bool
= fun lam vlist -> match lam with
			| V x -> find x vlist
			| P (x,l) -> checkfree l (x::vlist)
			| C (l1,l2) -> (checkfree l1 vlist) && (checkfree l2 vlist)

let rec check : lambda -> bool
= fun lam -> match lam with
			| V x -> false
			| P (x,l) -> checkfree l (x::[])
			| C (l1,l2) -> (check l1) && (check l2)