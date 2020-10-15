let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if a>b then f(a) + sigma f b (a-1)
else if a=b then f(b)
else f(b) + sigma f a (b-1)
 (* TODO *)