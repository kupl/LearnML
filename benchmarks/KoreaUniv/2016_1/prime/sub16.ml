let rec prime n =
	let rec nd k =
	n mod k <> 0 && nd (k+1) in
	n <> 1 && nd 2;;
