(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with 
|0 -> 0
|1 -> 1
|_ -> fib(n-1) + fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> (* TODO *)
if n1 = n2 then 1
else if n2 = 0 then 1
else pascal (n1-1, n2-1) + pascal (n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
if n = 2 then true
else let rec division d = if d*d > n then true
  else (n mod d) <> 0 && division (d+1) in division 2


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then f a
else sigma f a (b-1) + f b 

