(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n <= 1 then n else fib (n - 1) + fib (n - 2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 || n2 = n1 then 1 else pascal (n1 - 1, n2) + pascal (n1 - 1, n2 - 1);;

(* Problem 3 *)
let rec prime2 : int -> int -> bool
= fun n p -> if p <= 1 then true else if n mod p = 0 then false else (prime2 n (p-1));;

let rec prime : int -> bool
= fun n -> prime2 n (n-1);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f p q -> if p = q then (f p) else (f p) + (sigma f (p+1) q);;