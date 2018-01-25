
(* problem 4*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f a 
               else if a > b then 0
               else (f a) * (product f (a+1) b)

