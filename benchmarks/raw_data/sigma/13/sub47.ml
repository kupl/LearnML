let rec sigma = fun x ->
	match x with
	(a,b,f) -> if a>b then 0 else if a = b then (f b) else (f a) + (sigma ((a + 1),b,f));;


