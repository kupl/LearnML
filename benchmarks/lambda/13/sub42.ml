type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec checkByList (m, l) =
    match m with
    | V a -> List.exists (fun x -> x = a) l
    | P (n, a) -> checkByList (a, l @ [n])
    | C (a, b) -> checkByList (a, l) && checkByList (b, l)

let check m = checkByList (m, [])
