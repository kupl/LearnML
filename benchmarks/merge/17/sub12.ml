let rec merge ((ilist1: int list), (ilist2: int list)) : int list =
	match (ilist1,ilist2) with
	| ([], _) -> ilist2
	| (_, []) -> ilist1
	| (hd1::tl1, hd2::tl2) -> if hd1>hd2 then hd1::merge (tl1,ilist2) else hd2::merge(ilist1,tl2)