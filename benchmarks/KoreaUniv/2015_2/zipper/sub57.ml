let rec zipper : int list * int list -> int list
=fun(l1, l2) 
	-> match l1 with 
	|[]-> l2	
	|hd1::tl1
		-> (match l2 with 
		|[]->l1	
		|hd2::tl2 ->if hd1 < hd2 then [hd1]@[hd2]@(zipper(tl1,tl2))	
			else [hd2]@[hd1]@(zipper(tl1,tl2))
			)
