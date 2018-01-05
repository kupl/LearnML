let rec iter (n, f) x =
    match n with
    | _ when n < 0 -> failwith "Negative Number"
    | 0 -> x
    | n' -> iter (n'-1, f) (f x)
