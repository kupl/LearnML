(* Problem 1 *)
let rec fib : int -> int
= fun n ->
	if n = 0 then 0
	else if n = 1 then 1
	else fib (n-2) + fib (n-1)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
	if n2 = 0 then 1
	else if n1 = n2 then 1
	else pascal(n1-1, n2-1) + pascal(n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
	let rec test dividend divisor =
		if dividend <= 1 then false
		else if divisor < 2 then true
		else if dividend mod divisor = 0 then false
		else test dividend (divisor-1)
	in test n (n-1);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	let rec cal sum f now_i end_i =
		if now_i > end_i then sum
		else cal (sum + (f now_i)) f (now_i + 1) end_i
	in cal 0 f a b;;