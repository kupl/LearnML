let rec merge (ilst1, ilst2) =
	match ilst1 with
	| hd1::tl1 -> 
		(match ilst2 with
		| hd2::tl2 -> 
			if hd1 > hd2 then hd1::(merge (tl1, ilst2))
			else hd2::(merge (ilst1, tl2))
		| [] -> ilst1
		)
	| [] -> ilst2;;
