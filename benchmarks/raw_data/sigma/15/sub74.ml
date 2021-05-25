let rec iter_f a b f result =
    if a <= b then
        iter_f (a + 1) b f (result + (f a))
    else
        result

let sigma (a, b, f) = iter_f a b f 0
