(* Problem 1 *)

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a <= b then
(f a) + sigma f (a+1) b else 0;;
