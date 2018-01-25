(* problem 2*)
let smallest_divisor : int -> int
= fun n ->
	if n=1 then 1
	else if (n mod 2 = 0) then 2
	else let rec test x y = if(y*y > x) then x else if (x mod y =0) then y else (test x (y+2)) in
	test n 3;;