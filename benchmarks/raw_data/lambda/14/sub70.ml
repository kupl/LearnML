type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
and var = string

let rec check2 mt arealist =
    match mt with
    | V str ->
            List.exists (fun x -> x=str) arealist
    | C (m1,m2) ->
            (check2 m1 arealist) && (check2 m2 arealist)
    | P  (str, m) ->
            check2 m (str::arealist)

let check met =
    check2 met []
