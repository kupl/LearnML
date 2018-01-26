let rec prime : int -> bool
= fun n -> if n <= 1 then false 
           else if n <= 3 then true 
           else if n mod 2 = 0 then false 
           else if n mod 3 = 0 then false
	   else let rec other_test h = if h * h >= n then true 
					else if n mod h = 0 then false
					else other_test(h+2)
	   in other_test(5);;
