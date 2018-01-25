(* Problem 1 *)
let rec fib : int -> int
= fun n ->
	if			n = 0 then 0
	else if	n = 1 then 1
	else		fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
	if			n2 = 0	then 1
	else if	n1 = n2	then 1
	else		pascal(n1-1, n2-1) + pascal(n1-1, n2);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
	if			n = 1				then false
	else if n = 2				then true
	else if n mod 2 = 0 then false
	else
		let len = int_of_float(sqrt(float_of_int(n))) in
		let rec divide : int * int -> bool
		= fun (num, div) ->
			if			div > len				then true
			else if num mod div = 0 then false
			else		divide(num, div+2)
		in divide(n, 3)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
	if	 a = b then f a
	else (f a) + (sigma f (a+1) b);;
