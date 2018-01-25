(* problem 2*)
let rec small divnow n =
	if n mod divnow =0 then divnow
	else small (divnow+1) n;;
let smallest_divisor : int -> int
= fun n ->
small 2 n;;
