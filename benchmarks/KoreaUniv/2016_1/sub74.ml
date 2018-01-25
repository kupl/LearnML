(* Problem 1 *)
let rec fib : int -> int
= fun n ->  if (n = 0) then 1
else if (n = 1) then 1 else fib(n-1) + fib(n-2) 

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n1 = n2 || n2 = 0 then 1 
else pascal(n1-1, n2-1) + pascal(n1-1, n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let rec is_prime a = if (a = 1) then 1
else if (n mod a) = 0 then 0
else if ((is_prime (a-1)) = 1) then 1
else 0 in
if ((is_prime (n-1)) = 1 ) then true
else false
(* Problem 4 *)

let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f(b) else f(b) + sigma f a (b-1) 

