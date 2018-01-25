(* Problem 1 *)
let pascal : int * int -> int
=fun (x,y) -> (* TODO *)
  let rec _pascal (a, b) =
	match (a, b) with
	|(0,0)-> 1
	|_ -> if (b < 0)||(a < b) then
			0
		  else
		    _pascal(a-1,b-1) + _pascal(a-1,b)
		in _pascal(x,y)
;;
