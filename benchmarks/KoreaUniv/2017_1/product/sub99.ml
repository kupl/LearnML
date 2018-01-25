(* problem 4*)

let rec product : (int -> int) -> int -> int -> int
= fun f a b -> if a > b then raise(Failure "Error") else 
match (b-a) with
|0 -> f a
|_ -> (f a) * product f (a+1) b
