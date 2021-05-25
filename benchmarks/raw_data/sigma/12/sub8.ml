
let rec sigma (a, b, f) =
	if a > b then 0
	else if a == b then f a
	else (f a) + (sigma (a+1, b, f))
	;;

(* exercise test
Printf.printf "sigma : %d\n" (sigma(0, 10, function x -> x));;
exercise *)
