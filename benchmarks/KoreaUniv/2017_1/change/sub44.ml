(* problem 8*)

let rec change : int list -> int -> int
= fun coins amount -> match coins with
					| [] -> 0
					| hd::tl -> if amount<0 then 0 else
								if amount=0 then 0 else
if (amount-hd) = 0 then (1+ (change tl (amount-hd)))
								else if (amount-hd) > 0 then (change coins (amount-hd) + change tl amount)
								else change tl amount;;