
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f(b) else f(b) + sigma f a (b-1) 

