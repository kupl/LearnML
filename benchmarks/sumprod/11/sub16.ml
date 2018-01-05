let rec sigma(a,b,f) =
    if a>b then 0
    else if a=b then (f a)
    else (f a) + sigma(a+1,b,f)

let rec pi(c,d,e,g) =
    if d>e then 0
    else if d=e then (g(c,d))
    else (g(c,d)) * pi(c,d+1,e,g)

let sumprod(matrix, n, k) =
    let mat = function i -> pi(i,1,k,matrix) in
    sigma(1, n, mat)
