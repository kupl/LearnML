let rec sigma f a b  =
    if a>b then 0 else sigma f a (b-1) + f(b);;
