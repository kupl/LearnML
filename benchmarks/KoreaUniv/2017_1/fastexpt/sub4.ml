(* problem 1*)
let rec fastexpt : int -> int -> int
= fun b n -> 
if n= 0 then 1
else if n= 1 then b
else let c = n/2 
		in (if n mod 2 =0 then 1 else b)*(fastexpt b c)*(fastexpt b c)