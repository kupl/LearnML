(* problem 1*)
let square x = x*x
let iseven x = (x mod 2) = 0
exception Input_range

let rec fastexpt : int -> int -> int
= fun b n -> (*TODO*)
	if n<0 then raise Input_range
	else if n=0 then 1
	else if iseven n then square (fastexpt b (n/2))
	else b * (fastexpt b (n-1))