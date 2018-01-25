(* problem 2*)

let rec compare d n = 
	if (n mod d) = 0 then d 
	else (if (d*d) > n then n else (compare (d+2) n))

let smallest_divisor : int -> int
= fun n ->
	if (n mod 2) = 0 then 2
	else (compare 3 n)