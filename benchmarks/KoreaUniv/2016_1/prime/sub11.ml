let rec prime : int -> bool
= fun num -> if num<=1 then false else if num=2 then true else
			let rec div dd = if (dd*dd >num) then true else if (num mod dd= 0) then false else div(dd+1) in div 2;;

