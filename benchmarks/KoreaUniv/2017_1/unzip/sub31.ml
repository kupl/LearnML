(* problem 7*)

let join_tuple tuple1 tuple2 = 
	let (elt1, elt2) = tuple1 in
		let (elt3, elt4) = tuple2 in
			(elt1::elt3, elt2::elt4)

let rec unzip : ('a * 'b) list -> 'a list * 'b list
= fun lst ->
	match lst with
	|[] -> ([],[])
	|hd::tl -> join_tuple hd (unzip tl)