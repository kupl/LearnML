(* problem 8*)

let change : int list -> int -> int
= fun coins amount -> let rec f coins amount = if amount == 0 then 1 
                                               else if amount < 0 then 0 
                                               else match coins with 
	                                            |[] -> 0
		                                    |h::t -> f coins (amount-h) + f t amount in
                      f coins amount
