let rec sigma(a, b, f) =
    if a - b <= 0 then f(a) + (sigma((a+1), b, f))
    else 0
