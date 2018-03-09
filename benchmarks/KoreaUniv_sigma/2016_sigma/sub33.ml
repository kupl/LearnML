let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> (* TODO *)
if b>a then sigma f (a+1) b + f a
else f a
