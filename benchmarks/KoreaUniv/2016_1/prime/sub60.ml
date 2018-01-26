let rec prime : int -> bool
= fun n -> 
		let k = abs n in
		let rec not_divided_by d =
			d * d > k || (k mod d <> 0 && not_divided_by (d+1)) in
	k <> 1 && not_divided_by 2
