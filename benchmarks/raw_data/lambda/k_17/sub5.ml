			
(**********************)
(*   Problem 2        *)
(**********************)

type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

type variables = var list (*necessary?? var?? *)

let rec findVar : var list -> lambda -> bool
= fun vlist lam -> match lam with
		| V (x) -> (match vlist with
			| [] -> false 
			| hd::tl -> if hd = x then true else findVar tl lam)
		| P (v,l) -> findVar (v::vlist) l 
		| C (l1,l2) -> (findVar vlist l1)&&(findVar vlist l2)
	
let rec check : lambda -> bool
= fun lam -> findVar [] lam

(* 
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;
check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;
*)
