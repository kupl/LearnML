let sigma f a b  =
    let rec aux (a, b, f, result) =
        if a > b then result
        else aux( (a+1),  b,  f, result + (f a))
    in aux(a, b, f, 0);;
