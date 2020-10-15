(* Problem 1 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	if a>b then
		0
	else
		f b + sigma f a (b-1)
