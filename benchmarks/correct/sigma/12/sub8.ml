
let rec sigma f a b =
	if a > b then 0
	else if a == b then f a
	else (f a) + (sigma f (a+1) b)
	;;

(* exercise test
Printf.printf "sigma : %d\n" (sigma(0, 10, function x -> x));;
exercise *)
