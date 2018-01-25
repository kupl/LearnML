(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n <= 0 then 0 else if n = 1 then 1
 else fib(n - 1) + fib(n - 2)
 
(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 || n1 = n2 then 1
 else pascal(n1 - 1, n2 - 1) + pascal(n1 - 1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
	let rec isPrime a = if a * a <= n then n > 1 && n mod a <> 0
	&& isPrime (a + 1) else n > 1 in isPrime 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a > b then 0 else if a = b then f a
else f b + sigma f a (b - 1) 


