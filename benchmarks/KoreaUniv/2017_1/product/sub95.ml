
(* problem 4*) 
let rec product : (int -> int) -> int -> int -> int
= fun f a b ->
if b > a then (product f a (b-1)) * f (b)  
else f a

