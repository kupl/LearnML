(* problem 4-solve*)
let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if b=a then f b else f a*product f (a+1) b