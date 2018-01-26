let rec prime : int -> bool
= let rec check n d =
 	  if d > n-1 then true
		else if n mod d = 0 then false
		else check n (d + 1) in fun n -> check n 2
