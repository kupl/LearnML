(* problem 8*)
let rec change : int list -> int -> int
= fun coins amount -> if amount=0 then 1
		      else if amount<0 then 0
		      	   else match coins with
				| [] -> 0
				| hd::tl -> if amount < hd then (change tl amount)
					    else let rec sum last_amount = if last_amount<hd then 0
									   else (sum (last_amount-hd)) + (change tl (last_amount-hd))
						 in (change tl amount) + (sum amount);;
