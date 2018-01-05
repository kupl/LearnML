let rec sigma (a, b, f): int = 
    if a = b then f a
    else sigma (a+1, b, f) + f a
