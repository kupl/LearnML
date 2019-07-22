type metro = STATION of name
		   | AREA of name * metro
		   | CONNECT of metro * metro
and name = string

let checkMetro (m : metro) =
	let rec sub_check (m : metro) (id_list : name list) =
		match m with
		| STATION id ->
			let rec find id id_list =
				match id_list with
				| [] -> false
				| hd :: tl -> if hd = id then true else find id tl
			in find id id_list
		| AREA (id, m1) -> sub_check m1 (id :: id_list)
		| CONNECT (m1, m2) -> (sub_check m1 id_list)
								&& (sub_check m2 id_list)
	in sub_check m []
