(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
	let listHead : int list -> int
	= fun lst ->
		match lst with
		n::_ -> n
	|	[] -> (-1)
	in
	let rec findMax : int list -> int -> int
	= fun lst max ->
		match lst with
		n::t -> if max < n then findMax t n else findMax t max
	|	[] -> max
	in
	findMax lst (listHead lst)
	(*
	match max with
	List.fold_left (fun a b -> if a > b then a else b) max lst
	*)
 