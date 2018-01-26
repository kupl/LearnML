let prime n = 
	let d = n - 1 in
	let rec is_divisor n d = 
		if d > 1 && n mod d = 0 then false else
		if d = 2 && n mod d <> 0 then true else
		is_divisor n (d-1) in
	 		if n > 2 then is_divisor n d else
	 		if n = 2 then true
			else false;;
 