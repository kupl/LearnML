let rec max p = 
	match p with
	[t] -> t
	| hd::tl -> if hd > max tl then hd else
	max tl
 