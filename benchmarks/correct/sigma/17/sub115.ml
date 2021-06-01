let rec sigma f a b =
    let rec sigma2 (a, b, f, sum) =
        if a > b then sum
        else sigma2 (a+1, b, f, sum + (f a))
    in sigma2 (a, b, f, 0)
