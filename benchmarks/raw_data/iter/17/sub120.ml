let rec iter (n, f) = fun x -> if(n == 0) then x else iter((n-1), f) (f x)
