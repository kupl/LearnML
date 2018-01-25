(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> match n with
			|0 -> 1
			|_ -> match n mod 2 with
				|1 -> b* (fastexpt b ((n-1)/2))*(fastexpt b ((n-1)/2)) 
				|_ -> (fastexpt b (n/2))*(fastexpt b (n/2));;