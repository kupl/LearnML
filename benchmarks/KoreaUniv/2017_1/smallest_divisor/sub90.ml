(* problem 2 *)

let smallest_divisor : int -> int
= fun n -> 
if n = 1 then 1	
else let i = 2 in
	let rec checkundersquare a b = 
		if a >= (b*b) then
			if (a mod b) = 0 then b	
			else checkundersquare a (b+1)
		else a
	in checkundersquare n i
;;