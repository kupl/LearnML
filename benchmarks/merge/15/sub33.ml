
let rec merge l1 l2 = match l1, l2 with
	| [], _ -> l2
	| _, [] -> l1
	| hl1 :: tl1 , hl2 :: tl2 ->
		if hl1 > hl2 
			then hl1 :: merge tl1 l2
			else hl2 :: merge l1 tl2
