let rec iter ((n : int), (f : 'a ->'a)) (x:'a) : 'a =
    match (n,f) with
    |(0,_) -> x
    |(_,_) -> f(iter (n-1,f) x)
