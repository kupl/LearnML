type lambda = V of var
        | P of var * lambda
        | C of lambda * lambda
    and var = string

let check met =
    let rec checkInclude m l =
        match m with
        | V s -> List.mem s l
        | P (a, inner_m) -> checkInclude inner_m (a::l)
        | C (a, b) -> (checkInclude a l) && (checkInclude b l)
    in checkInclude met [];;