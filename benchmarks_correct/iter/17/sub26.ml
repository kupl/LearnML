let rec iter (n, f) x =
        match n with
        |0 -> x
        |_ -> iter (n-1, f) (f(x))
