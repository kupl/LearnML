let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	let rec sum now_sum f now_i end_i =
		match now_i > end_i with
		| true -> now_sum
		| false -> sum (now_sum + (f now_i)) f (now_i + 1) end_i
	in sum 0 f a b;;