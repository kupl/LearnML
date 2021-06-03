let rec sigma f a b  =
    if a - b <= 0 then f(a) + (sigma f (a+1) b)
    else 0
