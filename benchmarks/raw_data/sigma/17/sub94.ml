let rec sigma (a, b, func) =
    if a > b then 0
    else (func a) + sigma(a + 1, b, func)

