
(* problem 1*)
let fastexpt : int -> int -> int
= fun b n -> 
if n mod 2=0 then (b fastexpt (n/2)) fastexpt 2
		else b*(b fastexpt (n-1))
