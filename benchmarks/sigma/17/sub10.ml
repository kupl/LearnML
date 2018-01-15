let rec sigma' ((a: int), (b: int), (f: int->int), (subSum: int)): int =
    if a = b then f a + subSum
    else sigma' (a + 1, b, f, subSum + f a)

let sigma ((a: int), (b: int), (f: int->int)): int =
    if a > b then 0
    else sigma' (a, b, f, 0)
