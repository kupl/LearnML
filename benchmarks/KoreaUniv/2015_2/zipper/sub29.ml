let rec zipper : int list * int list -> int list
=fun (a,b) -> 
	match a with
	[]->b
	|h1::t1-> (match b with
		[]->a
		|h2::t2->h1 :: h2 :: zipper (t1,t2))
