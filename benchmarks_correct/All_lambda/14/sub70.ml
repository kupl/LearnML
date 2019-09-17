type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check mt arealist =
    match mt with
    | V str ->
            List.exists (fun x -> x=str) arealist
    | C (m1,m2) ->
            (check m1 arealist) && (check m2 arealist)
    | P  (str, m) ->
            check m (str::arealist)

let check met =
    check met []
