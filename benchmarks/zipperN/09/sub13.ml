let rec zipperN l =
	if l = [] then []
	else if List.hd l = [] then zipperN (List.tl l)
	else
		let head l1 =
			match l1 with
				[] -> 0
			| h::t -> h
		in
		let tail l2 =
			match l2 with
		  	[] -> []
			| h::t -> t
		in
		List.filter (fun x -> x!=0) (List.map head l) @ (zipperN (List.map tail l));;

(*let _ = List.map (fun x -> print_int x; print_newline()) (zipperN [[1;2;3];[4];[9;10;11]])*)