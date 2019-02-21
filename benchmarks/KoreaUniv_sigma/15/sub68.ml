let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->	if a > b then 0 else
				(f a) + sigma f (a + 1) b;;
