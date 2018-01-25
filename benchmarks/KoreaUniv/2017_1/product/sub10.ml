
(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> let rec loop x =
if x > b then 1
else (f x)*(loop (x+1))
in (loop a)
