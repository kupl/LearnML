let rec prime : int -> bool
	= fun n -> if n<2 then false
		else if n=2 then true
		else let rec div d =
			if (n mod d = 0 && n > d) then false
			else if (n=d) then true
			else div (d+1) in div 2;;

