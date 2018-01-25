let smallest_divisor : int -> int
= fun n -> let rec divisor a b 
				= if a mod b = 0 then b else (divisor a (b+1)) in
			let sq x = x*x in
			let x = 2 in if n > (sq x) then divisor n x
			else n