let rec sumprod(f, n, k) =
    let rec prod n k =
        if k <= 0 then 1.0
        else f(n, k) *. prod n (k - 1)
    in
    if n <= 0 then 0.0
    else prod n k +. sumprod(f, n - 1, k)
