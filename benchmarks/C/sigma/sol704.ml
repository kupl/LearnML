let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	let rec cal sum f now_i end_i =
		if now_i > end_i then sum
		else cal (sum + (f now_i)) f (now_i + 1) end_i
	in cal 0 f a b;;