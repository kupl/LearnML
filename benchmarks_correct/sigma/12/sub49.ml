let rec sigma f a b =
	match (a,b,f) with
	|(a,b,f) -> if (a>b) then 0
			else f a + sigma f (a+1) b
