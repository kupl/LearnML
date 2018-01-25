(* problem 1*)
let fastexpt : int -> int -> int
= fun b n ->
	let rec help b n =
		if n == 0 then 1
		else if n mod 2 == 0 then (help b (n/2)) * (help b (n/2))
		else b * (help b (n-1)) in
			help b n