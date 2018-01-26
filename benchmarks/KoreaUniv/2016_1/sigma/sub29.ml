let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a<b then (sigma f a (b-1)) + (f b) else f b 
