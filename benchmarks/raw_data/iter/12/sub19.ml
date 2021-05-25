(* HW1 exercise3 2009-11697 Kim HyunJoon*)
(* Iter *)

let rec iter : (int * ('a -> 'a)) -> 'a -> 'a =
	fun (n, f) x ->
	if n <= 0 then x
	else iter ((n-1), f) (f x)
