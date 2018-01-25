(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> 
if b - a = 0 then f a
else (f a)*(product f (a+1) b);;
