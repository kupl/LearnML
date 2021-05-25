let rec sigma : int * int * (int -> int) -> int = fun (a, b, g) ->
    if (a > b)
        then 0
        else if (a == b) 
            then (g a)
            else sigma (a+1, b, g) + (g a)
