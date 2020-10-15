let rec sigma g a b =
    if (a > b)
        then 0
        else if (a == b) 
            then (g a)
            else sigma g (a+1) b + (g a)
