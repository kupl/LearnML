let incr n = n + 1

let rec sigma f a b  = 
        if a > b then 0
        else if a = b then f a
        else f a + (sigma f (incr a) b)
