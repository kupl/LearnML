(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with (* TODO *)
|0 -> 0
|1 -> 1
|n -> fib(n-1) + fib(n-2)


(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> match n1, n2 with (* TODO *)
| n1, 0 -> 1
| n1, n2 when n1=n2 -> 1 
| n1, n2 -> pascal(n1-1, n2-1) + pascal(n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
	let rec divide a =
		if prime(a) then n mod a <> 0 && divide (a+1)
		else if a*a > n then true
		else divide (a+1) in 
	if n=1 then false
	else if n=2 then true
	else if n=3 then true
	else divide 2
(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->  (* TODO *)
if a=b then (f b) 
else (f a) + (sigma f (a+1) b)
