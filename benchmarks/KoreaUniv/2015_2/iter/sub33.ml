let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> let y = f in if n=0 then y else iter((n-1), f)
