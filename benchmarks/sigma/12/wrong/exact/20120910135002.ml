let rec sigma (i1, i2, f) =
        if (i1 > i2) then 0
        else if (i1 = i2) then (f i1)
        else ((f i1) + (sigma ((i1 + 1), i2, f)))

