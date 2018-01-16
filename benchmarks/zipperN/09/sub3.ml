let rec zipperN a =
	let not_empty lst = match lst with head::tail -> true | _ -> false in
	let filter_empty_lists lst = (List.filter not_empty lst) in
	let b = (filter_empty_lists a) in	
	match b with
		| [] -> []
		| _ ->	(List.map List.hd b) @ (zipperN (List.map List.tl b))
;;
