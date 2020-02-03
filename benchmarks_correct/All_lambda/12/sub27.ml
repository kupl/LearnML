(* hw1-8 *)
(* 2010-11687 Keunjun choi *)

type lambda = V of var
                  | P of var * lambda
                  | C of lambda * lambda
and var = string
let check m =
	let rec check2 (ms, l) =
		match ms with
		| V a -> List.mem a l
		| P (a, b) -> check2 (b, a::l)
		| C (a, b) -> check2 (a, l) && check2 (b, l)
	in
	check2 (m, [])
