let rec zipper : int list * int list -> int list
=fun (a,b) -> 
	match a with
	| [] -> b
	| hd::tl ->
		match b with
			|[] -> a
			|h::t -> hd::h::zipper (tl,t);;
 (* TODO *)
