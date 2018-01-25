(* Problem 1 *)
let rec fib : int -> int
= fun n ->
	match n with
	| 0 -> 0
	| 1 -> 1
	| _ -> fib (n-2) + fib (n-1)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
	match n2 = 0 || n1 = n2 with
	| true -> 1
	| false -> pascal(n1-1, n2-1) + pascal(n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
	let rec divide dividend divisor =
		if dividend <= 1 then false
		else if divisor < 2 then true
		else 
			match dividend mod divisor with
			| 0 -> false
			| _ -> divide dividend (divisor-1)
	in divide n (n-1);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	let rec sum now_sum f now_i end_i =
		match now_i > end_i with
		| true -> now_sum
		| false -> sum (now_sum + (f now_i)) f (now_i + 1) end_i
	in sum 0 f a b;;