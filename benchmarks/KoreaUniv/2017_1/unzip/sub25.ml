let rec unzip : ('a * 'b) list -> 'a list * 'b list = fun lst ->
	match lst with
	[] -> ([],[])
	|(a,b)::tl -> let (l1,l2) = unzip tl in (a::l1,b::l2);;
