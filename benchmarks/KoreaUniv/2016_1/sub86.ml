(* Problem 1 *)
let rec fib : int -> int
= fun n -> if n = 0 then 0 else if n = 1 then 1 else n + fib (n-1);; (* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> if n2 = 0 then 1 else if n2 = n1 then 1 else pascal (n1-1 , n2-1) + pascal (n1-1, n2);; (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
let rec inpr : int -> bool
= fun k -> 
if k = n then true else if n mod k = 0 then false else inpr (k+1)  
in inpr 2;; (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a else f a + sigma f (a+1) b;; (* TODO *)