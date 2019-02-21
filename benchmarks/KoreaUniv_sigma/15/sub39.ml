let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->
		if b=0 then 0
		else if a>b then 0
		else if a=b then f a
		else ((f a) + sigma f (a+1) b)
