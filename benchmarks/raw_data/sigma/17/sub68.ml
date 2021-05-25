let rec sigma : int * int * (int -> int) -> int =
	fun (a,b,f) -> if a==b then f a
			else f a + sigma(a+1,b,f)

