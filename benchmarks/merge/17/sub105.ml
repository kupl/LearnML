let rec merge ((list1 : int list), (list2 : int list)) : int list =
	match (list1,list2) with
	| ([], _) -> list2
	| (_, []) -> list1
	| (hd1::tl1, hd2::tl2) -> 
		if hd1 >= hd2 then hd1::(merge (tl1, hd2::tl2))
		else hd2::(merge (hd1::tl1, tl2))