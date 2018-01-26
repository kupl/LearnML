let rec prime : int -> bool
= fun n -> if n <= 1 then false else
			if n=2 then true else
			let rec div d =
			if (d*d >n) then true else 
				if (n mod d = 0) then false else div(d+1) in div 2;;

