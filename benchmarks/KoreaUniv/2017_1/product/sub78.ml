
(* problem 4*)

let product : (int -> int) -> int -> int -> int
= fun f a b -> 
 |a=0
 |if b=0 then 1
 |else if b>1 then f(b-1) + b
 |prof