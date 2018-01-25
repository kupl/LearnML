let rec zipper : int list * int list -> int list  =
	fun (a, b) -> match (a, b) with
	([], []) -> []	
	|(a, []) -> a
	|([], b) -> b
	|(h1::t1, h2::t2) -> if h1 > h2 then h2::zipper (a, t2) else h1::zipper (t1, b)
