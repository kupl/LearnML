type lambda = V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let check : lambda -> bool = fun m ->
    let rec sub_check : lambda -> string list -> bool = fun m l ->
        match m with
        | V n -> List.exists (fun x -> x = n) l
        | P (n, m) -> sub_check m (n::l)
        | C (m1, m2) -> (sub_check m1 l) && (sub_check m2 l)
    in  
    sub_check m []

