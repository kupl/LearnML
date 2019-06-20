type t = (int * btree)
and btree = Empty | Node of int * btree * btree 

let to_string : t -> string
= fun (n, t) -> 
	let rec string_of_btree : btree -> string 
	= fun t ->
		match t with
		| Empty -> "Empty"
		| Node (n, l, r) -> "Node (" ^ string_of_int n ^ "," ^ string_of_btree l ^ "," ^ string_of_btree r ^ ")"
	in
	string_of_int n ^ ";" ^ string_of_btree t

let shrink : t -> t QCheck.Iter.t 
= fun (n, t) -> 
  	let open QCheck.Iter in
  	let rec shrink_btree t =
		match t with
		| Empty -> empty
		| Node (n, l, r) -> 
			(of_list [l; r]) 
			<+> (map (fun l' -> Node (n, l', r)) (shrink_btree l)) 
			<+> (map (fun r' -> Node (n, l, r')) (shrink_btree r)) 
			<+> (map (fun n' -> Node (n', l, r)) (QCheck.Shrink.int n))
	in
	pair (QCheck.Shrink.int n) (shrink_btree t)

let gen : t QCheck.Gen.t 
= 
	let open QCheck.Gen in
	let gen_btree = 
		sized (fix 
		(fun recgen n ->
			match n with
			| 0 -> return Empty
			| _ -> 
				frequency [
				 1, return Empty; 
				 3, map3 (fun n l r -> Node (n, l, r)) small_int (recgen (n/2)) (recgen (n/2))
				])
		)
	in
	pair small_int gen_btree