
(* Problem 1 *)
let rec fib : int -> int
= fun n -> match n with
0 -> 0 | 1 -> 1 | _ -> fib (n - 1) + fib(n - 2) (* TODO *)

(* Problem 2 *)
let rec pascal : int * int -> int
= fun (n1, n2) -> (
if n1 = 0 then 1 else
if n2 = 0 then 1 else
if n1 = n2 then 1 else
pascal(n1 - 1, n2 - 1) + pascal(n1 - 1, n2)) (* TODO *)

(* Problem 3 *)
let rec prime : int -> bool
= fun n ->
let rec dividing d =
d * d > n || (n mod d != 0 && dividing (d+1)) in
n != 1 && (dividing 2) (* TODO *)

(* Problem 4 *)
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a > b
then 0
else (f a) + (sigma f (a+1) b) (* TODO *)
