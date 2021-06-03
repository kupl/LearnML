exception BoundError

let rec sigma f a b =
        if a<b then sigma f (a+1) b + f a
        else if a=b then f a
        else raise BoundError
