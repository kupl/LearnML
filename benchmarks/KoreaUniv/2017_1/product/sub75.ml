
(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if b>=a then (f a) * (product f (a+1) b) else 1;;
