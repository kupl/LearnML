let rec prime : int -> bool 
= fun n ->  if n <= 1 then false
		else let rec f a = if (a = 1) then true
		else if (n mod a) = 0 then false
		else if ((f (a-1)) = true) then true
	        else false
		in
		f (n-1)
           