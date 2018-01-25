(* Problem 1 *)
let rec fib : int -> int
= fun n ->
match n with
0	-> 0
|1	-> 1
|_	-> (fib (n - 1)) + (fib (n - 2))
;;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
if (n2 = 0) || (n1 = n2) then 1 else
	(pascal (n1 - 1, n2 - 1)) + (pascal (n1 - 1, n2))
;;

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
let rec primep = fun n x y ->
if x <= 0 then false else
	if x = 1 then true else
		if n = x * y then false else 
			if x < y then primep n (x - 1) 2 else
				primep n x (y + 1)
in
primep n (n - 1) 2
;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if a > b then 0 else
	sigma f a (b - 1) + (f b)
;;

