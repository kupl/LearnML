type lambda = V of var
		   | P of var * lambda
		   | C of lambda * lambda
and var = string

let check (m : lambda) =
	let rec sub_check (m : lambda) (id_list : var list) =
		match m with
		| V id ->
			let rec find id id_list =
				match id_list with
				| [] -> false
				| hd :: tl -> if hd = id then true else find id tl
			in find id id_list
		| P (id, m1) -> sub_check m1 (id :: id_list)
		| C (m1, m2) -> (sub_check m1 id_list)
								&& (sub_check m2 id_list)
	in sub_check m []
