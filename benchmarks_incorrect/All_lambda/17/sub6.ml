type lambda = V of var
            | P of var * lambda
            | C of lambda * lambda
and var = string

let rec isInclude: var * lambda -> bool = fun (n, m) ->
    match m with
    | V a -> (n == a)
    | P (a, b) -> (isInclude (a, b)) && (isInclude (n, b))
    | C (a, b) -> (isInclude (n, a)) || (isInclude (n, b))

let check: lambda -> bool = fun x ->
    match x with
    | P (a, V b) -> (a == b)
    | P (a, b) -> isInclude(a, b)
    | _ -> false
