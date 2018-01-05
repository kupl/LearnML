
let rec sigma(st, ed, f ) = 
	if st>ed then 0
	else if st=ed then f st
	else f st + sigma(st+1,ed, f )


(*	
let _ = print_int (sigma(1,5, fun x -> x+2));
	print_newline()


let _ = print_int (sigma(5,5, fun x -> x+2));
	print_newline()
let t1 = (sigma(5,4, fun x -> x+2))
let _ = print_int(t1);
	print_newline()
*)	
