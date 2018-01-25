(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with
0 -> 0
| 1 -> 1
| _ -> fib (n-1) + fib (n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) ->
if n2 = 0 then 1
else if n1 = n2 then 1
else pascal (n1 -1, n2 -1) + pascal (n1 -1, n2);;

(* Problem 3 *)
let rec isprime : int * int -> bool
= fun (n,d) -> 
if d = 1 then true
else if n mod d = 0 then false
else isprime (n, d-1);;

let rec prime : int -> bool
= fun n -> if n = 1 then false else isprime (n, n-1);;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a > b then 0 
else if a = b then f a
else f a + sigma f (a+1) b;;

