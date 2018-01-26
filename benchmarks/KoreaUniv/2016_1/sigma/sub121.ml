let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if a = b then f b
else sigma f a (b - 1) + sigma f b b;;


