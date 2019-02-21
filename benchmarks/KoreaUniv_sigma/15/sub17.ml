(* Problem 1 *)
(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b ->if a=b then f b else f b+sigma f a (b-1);;
