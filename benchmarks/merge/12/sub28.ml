let merge (l1, l2) = 
	let rec mcore list1 list2 nowl =
		match (list1,list2) with
		|((hd1:int)::tl1,(hd2:int)::tl2) -> if (hd1 > hd2) then (mcore tl1 list2 (hd1::nowl))
								else if (hd1 = hd2) then (mcore tl1 tl2 (hd1::nowl))
								else (mcore list1 tl2 (hd2::nowl))
		|([],(hd:int)::tl) -> mcore [] tl (hd::nowl)
		|((hd:int)::tl,[]) -> mcore tl [] (hd::nowl)
		|([],[]) -> nowl
	in
	List.rev (mcore l1 l2 [])

