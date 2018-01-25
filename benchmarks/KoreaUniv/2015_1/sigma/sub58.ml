let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> let rec sigma f a b =
   if a=b then a
   else b + sigma f a b-1;;
   