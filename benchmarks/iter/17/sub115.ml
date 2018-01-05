let rec iter (n, f) =
    match n with
    | _ when n < 0 -> failwith "Nagetive Number"
    | 0 -> fun x -> x
    | n' -> fun x -> iter(n'-1,f) (f x)
