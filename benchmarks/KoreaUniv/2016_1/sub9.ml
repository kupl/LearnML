(* Problem 1 *)
let rec fib : int -> int
=fun n-> match n with
0 -> 0| 1 -> 1 | _ -> fib(n-1) + fib(n-2);;

(* Problem 2 *)
let rec pascal : int * int -> int
=fun(n1, n2) -> if n1 = n2 || n2 = 0 then 1
else pascal (n1-1, n2) + pascal (n1-1, n2-1);;

(* Problem 3 *)
let rec prime : int -> bool
= fun n -> let rec div d =
d*d > n || (n mod d <> 0 && div(d+1)) in
n<>1 && div 2;;

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a
else sigma f (a+1) b + sigma f a a;;