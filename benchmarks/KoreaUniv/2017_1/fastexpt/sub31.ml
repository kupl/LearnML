(* problem 1*)

let rec fastexpt : int -> int -> int
= fun b n -> 
	if n < 0 then raise (Failure "minus n") else
	match n with
	|0 -> 1
	|1 -> b
	|_ -> if (n mod 2 = 0) then (fun a -> a*a) (fastexpt b (n/2))
		  else b * (fastexpt b (n-1))