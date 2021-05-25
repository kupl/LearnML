let rec iter (n, f) = fun x ->
	if n<=0 then x
	else iter (n-1, f) (f x)

(* Test Code ::
let square n = n*n
let _ = print_int (iter(0, function x -> 2+x) (-3)); print_char '\n'
let _ = print_int (iter(7, function x -> 2+x) 0); print_char '\n'
let _ = print_int (iter(3, square) 2); print_char '\n'
let _ = print_int (iter(10, function x -> 2*x) 1); print_char '\n'
*)
