let rec max p = 
	match p with
	[t] -> t
	| hd::tl -> if hd > max tl then hd else
	max tl
let rec min p =
	match p with
	[t] -> t
	| hd::tl -> if hd < min tl then hd else
	min tl
