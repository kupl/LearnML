let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if b>a then sigma f (a+1) b + sigma f a a
else f a
