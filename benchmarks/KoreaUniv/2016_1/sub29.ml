(* Problem 1 *)
let rec fib : int -> int
=fun n -> if n<=1 then n else fib(n-1)+fib(n-2)

(* Problem 2 *)
let rec pascal : int * int -> int
=fun (n1, n2) -> if n2=0 || n2=n1  then 1 else pascal(n1-1, n2-1) + pascal(n1-1,n2)

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> if n = 1 then false
 else md n 2 and
 md n k = if n=k then true
 else if n mod k=0 then false
 else md n (k+1)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a<b then (sigma f a (b-1)) + (f b) else f b 
