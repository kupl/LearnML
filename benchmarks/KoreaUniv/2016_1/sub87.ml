(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with
	|0 -> 0
	|1 -> 1
	|_ -> fib (n - 1) + fib (n - 2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 then 1
	else if n1 = n2 then 1
	else pascal (n1-1, n2-1) + pascal (n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->

	let rec check a =
	if a = 1 then true
	else if n mod a = 0 then false
	else true && check (a-1) in

	if n = 1 then false
	else if n = 2 then true
	else check (n-1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f b 
	else f a + sigma f (a+1) b
