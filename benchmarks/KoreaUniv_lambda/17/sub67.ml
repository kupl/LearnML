(*	Problem 2	*)

type lambda = V of var
	| P of var * lambda
	| C of lambda * lambda
and var = string

let check :  lambda -> bool
= fun lam ->
	let rec reccheck : lambda -> var list -> bool
	= fun l vlist ->
		let rec listfind : var -> var list -> bool
		= fun x lst ->
			(match lst with
				| [] -> false
				| hd::tl -> if hd = x then true else listfind x tl
			)
		in
		match l with
			| V v -> listfind v vlist
			| P (v, l') -> reccheck l' (v::vlist)
			| C (l1, l2) -> (reccheck l1 vlist) && (reccheck l2 vlist)
	in
	reccheck lam []

