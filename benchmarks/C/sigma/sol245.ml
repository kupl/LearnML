let rec sigma f a b  = 
        if a-b > 0 then 0
        else (f a) + (sigma f (a+1) b)

