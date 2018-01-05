let merge : int list * int list -> int list = fun (l1, l2) ->
	let rec merge_ l1 l2 ret = match l1 with
		| [] -> ret@l2
		| hd::tl -> (match l2 with
			| [] -> ret@l1
			| hd'::tl' ->
				if (hd > hd')
				then (merge_ tl tl' (ret@hd::hd'::[]))
				else (merge_ tl tl' (ret@hd'::hd::[]))) in
	merge_ l1 l2 []