let rec prime : int -> bool
= fun n ->
		let rec divide = fun i n -> 
			if i=1 then true
			else if (n mod i) = 0 then false
			else divide (i-1) n in
				if n=1 then false else divide (n-1) n 
					