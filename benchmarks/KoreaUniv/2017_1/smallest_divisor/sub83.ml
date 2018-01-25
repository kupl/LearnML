(* problem 2*)

let smallest_divisor : int -> int
= fun n ->
	 let rec div n d = if (d*d)>n then n
										 else if (n mod d)=0 then d
  			  					 else div n (d+1)
   in div n 2;;