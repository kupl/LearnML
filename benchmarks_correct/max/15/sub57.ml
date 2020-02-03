
(* Problem 3 *)
let rec max : int list -> int
=fun l ->
 match l with
 hd::tl -> if tl = [] then
			hd
		   else
			  let mx = max(tl) in
				if hd > mx then
					hd
				else
					mx
;;
			  
  