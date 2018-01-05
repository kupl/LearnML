let rec zipper ( (a:int list), (b:int list) ) =
	match a with
	[] -> b
	| ha :: ta ->
		match b with
		[] -> a
		| hb :: tb -> [ha;hb] @ (zipper (ta, tb))
