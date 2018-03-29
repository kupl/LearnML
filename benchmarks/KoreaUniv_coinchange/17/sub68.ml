(*problem 8*)

let rec change : int list -> int -> int
=fun coins amount ->
match coins with 
| []-> if amount=0 then 1
	   else 0
| hd::tl -> if amount=0 then 1
		   	else if amount >0 then change coins (amount-hd) + change tl amount
		   	else 0
