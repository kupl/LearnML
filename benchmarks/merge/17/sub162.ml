let rec merge (listf, listl) =
	match (listf, listl) with
	|([], _) -> listl
	|(_, []) -> listf
	|(hd1::tl1, hd2::tl2) ->
	(if hd1 >= hd2 then hd1::merge (tl1, listl)
	else hd2::merge (listf, tl2)
	)
 