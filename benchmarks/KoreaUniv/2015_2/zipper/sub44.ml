let rec insert x 1 =
	match 1 with
	| [] -> [x]
	| hd::t1 -> if x < hd then x::hd::tl
		else hd::insert x t1

let rec zipper ((a:int list),(b: int list)) =
	match a with 
	| [] -> b
	| hd::t1 -> insert hd (zipper (t1,b));;
