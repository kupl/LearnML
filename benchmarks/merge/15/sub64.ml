let rec merge = fun (l1,l2)->
	match l1 with 
	|x1::t1 -> 
		(match l2 with
		|x2::t2 -> 
			if x1>x2 then x1::merge(t1, l2)
			else x2::merge(l1, t2)
		|[] -> l1)
	|[] -> l2


