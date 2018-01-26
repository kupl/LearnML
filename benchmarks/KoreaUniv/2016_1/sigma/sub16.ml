let rec sigma f a b =
	let rec f = (fun n -> n)  in
	if
	a = b then f a
	else f a  + sigma f a (b-1);;
