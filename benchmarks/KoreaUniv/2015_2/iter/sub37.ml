let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
        if (n<0) then raise (Failure "error") else
        match n with 
        | 0 -> ( fun x -> x)
        | n -> ( fun x -> f (iter(n-1,f) x))
