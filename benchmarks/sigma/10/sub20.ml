exception Error of string;;

(* Problem 1.*)
let sigma (a,b,f) =
function (a,b,f) ->
	if (a = b) then (f a)
		else if (a < b) then ((f a) + (sigma_temp ((a+1), b,f)))
			else raise (Error "Start point is bigger than end point")
;;