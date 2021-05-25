(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
	if n < 0 then -1
	else match n with
	| 0 -> 0
	| 1 -> 1 
	| _ -> fib (n - 1) + fib (n - 2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
	if n1 < 0 || n2 < 0 then -1
	else if n2 = 0 || n1 = n2 then 1
	else pascal (n1 - 1, n2 - 1) + pascal(n1 - 1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
	let rec cal (n1 : int) (n2 : int) : bool = 
		if n2 = 1 then true else 
		if n1 mod n2 = 0 then false
		else cal n1 (n2-1)
	in if n <= 1 then false else cal n (n-1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	let sum = 0 
	in if a > b then sum 
	else let sum = sum + f(a) + sigma f (a + 1) b in sum