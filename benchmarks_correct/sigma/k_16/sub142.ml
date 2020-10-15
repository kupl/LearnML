let rec sigma : (int -> int) -> int -> int -> int
= fun f a b -> let rec sum i z = 
if i = b+1  then z
else sum (i+1) (z+f i) in sum a 0

