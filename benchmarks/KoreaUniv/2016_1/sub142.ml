(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with
|0 -> 0
|1 -> 1
|n -> fib (n-1) + fib (n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if (n1, n2) = (0, 0) then 1
else if (n1, n2) = (n1, 0) then 1
else if (n1, n2) = (n1, n1) then 1
else pascal (n1-1, n2-1) + pascal(n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let n = abs n in 
let rec is_not_divisor d = 
d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in 
n <> 1 && is_not_divisor 2

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> let rec sum i z = 
if i = b+1  then z
else sum (i+1) (z+f i) in sum a 0

