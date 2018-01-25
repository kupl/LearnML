let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) a ->
if n=0 then a
else if n > 0 then f (iter(n-1,f) a)
else raise (Failure "n should be >=0")
