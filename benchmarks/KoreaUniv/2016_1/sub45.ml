(* Problem 1 *)
let rec fib : int -> int
= fun n -> 
match n with
0 -> 0
|1 -> 1
|_ -> fib (n-1) + fib (n-2);;


(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> 
if n2 = 0 then 1 else if n2 = n1 then 1 else pascal (n1-1, n2) + pascal(n1-1, n2-1);;


(* Problem 3 *)
let rec prime : int -> bool
= fun n -> 
let x = n-1 in
let rec rp : int -> bool
= fun i ->
if i = 1 then true else if n mod i = 0 then false else rp (i-1)
in if n = 1 then false else rp x;;


(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if a = b then f a else f a + sigma f (a+1) b;;

