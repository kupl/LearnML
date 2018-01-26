let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a=b then (f a)
	else (f a) + sigma f (a+1) b;;
