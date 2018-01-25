(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 1
		else if n = 1 then 1
		else fib(n - 1) + fib(n - 2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
(*if n1 = 0 && n2 = 0 then 1
else if n1 = 1 && n2 = 0 then 1
else if n1 = 1 && n2 = 1 then 1*)
if n1 = n2 then 1
else if n2 = 0 then 1
else pascal(n1 - 1, n2 - 1) + pascal(n1 - 1, n2)

(* Problem 3 *)
let prime : int -> bool
= fun n ->
	let n = abs n in
    let rec divisor d =
      d * d > n || (n mod d <> 0 && divisor (d+1)) in
    n <> 1 && divisor 2;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
	let n = abs a in
		if n <= b then (f n) + sigma f (n+1) b
	else 0
