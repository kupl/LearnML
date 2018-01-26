exception Invalid;;
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	if a > b then raise Invalid
	else
		match b with
			x when x = a -> (f a)|
			_ -> (f b) + (sigma f a (b-1));;
