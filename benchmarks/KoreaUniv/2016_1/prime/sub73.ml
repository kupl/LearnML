let rec test original yaksu = if yaksu = 1 then true
													else if original mod yaksu = 0 then false
													else test original (yaksu-1)
let rec prime : int -> bool
= fun n -> if n = 1 then false
						else test n (n-1)