exception Error of string
let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> 
if a>b then raise(Error "invalid arg")
else if a=b then (f a)
else (f b) + sigma f a (b-1)

