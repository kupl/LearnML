let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a > b then 0
else if b=a then f a else (f b) + (sigma f a (b-1))


