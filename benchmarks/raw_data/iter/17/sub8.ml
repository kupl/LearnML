let rec iter : (int * ('a -> 'a)) -> ('a -> 'a) = fun (n, f) ->
    if (n > 0) 
        then (fun x -> iter (n-1, f) (f x))
        else (fun x -> x)
