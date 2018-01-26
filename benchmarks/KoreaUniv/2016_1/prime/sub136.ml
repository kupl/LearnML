let rec prime : int -> bool
= fun n ->
	if n=1 then false
	else if n=2 then true
	else
		let rec primechk = fun a b ->
			if b=1 then true
			else if a mod b == 0 then false
			else primechk a (b-1) 
		in
		primechk n (n-1);;
