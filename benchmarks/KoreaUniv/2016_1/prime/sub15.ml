let rec prime : int -> bool
= fun n ->
	if n<=1 then false
	else if n<=3 then true
	else if (n mod 2 = 0 || n mod 3 = 0) then false
	else 
		let rec find i n =
		if i*i<= n then
			if (n mod i = 0) || (n mod (i+2) =0) then false
			else find (i+6) n
		else true
	in find 5 n
		