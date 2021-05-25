
let rec iter (n, f) =
	if n == 0 then function x -> x
	else function x -> iter(n-1, f) (f x)
	;;

(* exercise test
Printf.printf "iter : %d\n" (iter(2, function x -> x * 2) 2);;
exercise *)
