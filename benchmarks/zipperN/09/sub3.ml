let rec zipperN a =
	let not_empty list = match list with head::tail -> true | _ -> false in
	let filter_empty_lists list = (List.filter not_empty list) in
	let b = (filter_empty_lists a) in	
	match b with
		| [] -> []
		| _ ->	(List.map List.hd b) @ (zipperN (List.map List.tl b))
;;