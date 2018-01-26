let rec prime : int -> bool
= fun n -> 
	if n<=1 then false
	else

	let rec divide a =
		if a=1 then true 	 
		else  if n mod a=0 then false 
		else divide (a-1)   
	in divide (n-1);;
