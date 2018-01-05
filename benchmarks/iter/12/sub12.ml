let rec iter (n, f) =
        if n < 0 then raise (Invalid_argument "iter")
        else if n = 0 then (function x->x)
        else (function x -> iter (n-1, f) (f x))
