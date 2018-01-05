let rec merge: int list * int list -> int list  =
	fun (alist, blist) ->
	match (alist, blist) with
	| ([], _) -> blist
	| (_, []) -> alist
	| (hd1::tl1, hd2::tl2) -> if hd1>hd2 then hd1::merge(tl1,blist) else hd2::merge(alist, tl2)
