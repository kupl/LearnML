let rec sigma f i1 i2 =
        if (i1 > i2) then 0
        else if (i1 = i2) then (f i1)
        else ((f i1) + (sigma f (i1+1) i2))
