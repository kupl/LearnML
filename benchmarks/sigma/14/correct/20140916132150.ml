let rec sigma (a, b, f) = 
        if a-b > 0 then 0
        else (f a) + (sigma (a+1, b, f))

