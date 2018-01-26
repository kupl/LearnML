(* Problem 1 *)
exception Problem

let prime n =

	if n<=0 then raise Problem
	else

if n == 1 then false
else 
let rec undivi d =
d>=n || n mod d <>0 && undivi (d+1) in undivi 2;;
