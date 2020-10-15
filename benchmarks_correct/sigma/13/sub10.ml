
let rec sigma f st ed =
	if st>ed then 0
	else if st=ed then f st
	else f st + sigma f (st+1) ed


(*	
let _ = print_int (sigma(1,5, fun x -> x+2));
	print_newline()


let _ = print_int (sigma(5,5, fun x -> x+2));
	print_newline()
let t1 = (sigma(5,4, fun x -> x+2))
let _ = print_int(t1);
	print_newline()
*)	
