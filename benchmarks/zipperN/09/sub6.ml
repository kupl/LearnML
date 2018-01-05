let rec zipperN ( lst : int list list ) =
	let getTails y x = 
			match y with
			h::t -> t::x |
			[] -> x in
	let getHeads y x =
			match y with
			h::t -> h::x |
			[] -> x in
	match lst with
		h::t -> ( List.fold_right getHeads lst [] ) @ ( zipperN( List.fold_right getTails lst [] ) ) |
		[]  -> [];;
