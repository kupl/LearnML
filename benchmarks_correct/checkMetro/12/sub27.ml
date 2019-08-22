(* hw1-8 *)
(* 2010-11687 Keunjun choi *)

type lambda = V of var
                  | P of var * lambda
                  | C of lambda * lambda
and var = string
let check m =
	let rec check (ms, l) =
		match ms with
		| V a -> List.mem a l
		| P (a, b) -> check (b, a::l)
		| C (a, b) -> check (a, l) && check (b, l)
	in
	check (m, [])
