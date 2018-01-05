let rec sigma (a, b, f) = 
	if a>b then 0
	else if a=b then f a
	else (f a) + sigma (a+1, b, f)

let l1=sigma(1, 10, function x->x)

let _ =
	print_int l1;
	print_newline()
