let rec iter(n, f) =
    let identity = (fun x -> x) in
    match n with
    | 0 -> identity
    | _ -> fun x -> iter(n-1, f) (f x);;
