exception Invalid;;

let rec prime : int -> bool
= fun n ->
	if (abs n) = 1 then false
	else if n = 0 then raise Invalid
	else let  m = (abs n) in
		let rec is_divided d =
			if (d*d > m) then true
			else if (m mod d = 0) then false
			else is_divided (d+1) in
			 is_divided 2;;
