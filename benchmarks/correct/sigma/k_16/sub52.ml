let rec sigma : (int -> int) -> int -> int -> int
= fun f a b ->
if a = b then (f a)
else if a > b then raise (Failure "Invalid Input")
else ((f a) + sigma f (a+1) b)