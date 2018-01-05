let rec iter (n ,f) = function x ->
    match n with
    | 0 -> x
    | _n -> f (iter (_n-1, f) x);;
