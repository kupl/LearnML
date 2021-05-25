let rec iter : int * ('x->'x) -> ('x->'x) = fun p->
    match p with
    |(a, b) ->
        if (a == 0) then fun x -> x
        else fun x -> (b (iter(a-1,b) x))