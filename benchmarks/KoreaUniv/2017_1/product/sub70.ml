
(*problem 4*)
let product : (int -> int) -> int -> int -> int
= fun f a b -> 
let rec prod f a b =
if b > a then f(f b) * prod f a (b-1)
else f(f a) in prod f a b;;
