let rec sigma f a b = 
        if a > b then raise (Failure "sigma")
        else if a=b then f(a)
        else f(a) + (sigma f (a+1) b)

