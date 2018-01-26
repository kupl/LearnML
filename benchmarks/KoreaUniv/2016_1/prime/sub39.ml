let rec prime n =
	let rec div d = 
		d * d > n || (n mod d <> 0 && div(d+1)) in
		n <> 1 && div 2;;
