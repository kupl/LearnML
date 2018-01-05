let iter (n ,f) = fun x ->
    let rec aux (n, result) =
        match n with
        | 0 -> result
        | _n -> aux(_n-1, f result)
    in aux (n, x);;
