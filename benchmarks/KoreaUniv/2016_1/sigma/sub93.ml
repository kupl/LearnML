let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> if b=a-1 then 0
else sigma f a (b-1) + f b

