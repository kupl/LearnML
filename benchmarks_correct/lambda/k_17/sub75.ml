(**********************)
(* Problem 2*)
(**********************)

type lambda = V of var
| P of var * lambda
| C of lambda * lambda
and var = string

let rec find2 v m =
	match m with 
		|[] -> false
		|hd::tl -> if (hd = v) then true else find2 v tl

let addMem2 v m = v::m

let rec check2 : lambda -> string list -> bool
= fun lam mem -> 	
	match lam with
		| V v -> find2 v mem
		| P (v, l) -> 
			let mem2 = addMem2 v mem in
			check2 l mem2
		| C (l1, l2) -> check2 l1 mem && check2 l2 mem

let rec check : lambda -> bool
= fun lam -> check2 lam []