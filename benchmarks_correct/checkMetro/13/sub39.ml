type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
    and var = string

let check m =
    let rec checkM (m, l) =
        match m with
        | V n -> List.mem n l
        | P (var, lambda) -> checkM (lambda, var::l)
        | C (lambda1, lambda2) -> checkM (lambda1, l) && checkM (lambda2, l)
    in
    checkM (m, [])
