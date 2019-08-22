type lambda = V of var
    | P of var * lambda
    | C of lambda * lambda
and var = string

let check lambda = 
    let rec rec_check m lst =
        match m with
        | V var -> List.mem var lst
        | P (var, m1) -> rec_check m1 (var::lst)
        | C (m1, m2) -> (rec_check m1 lst) && (rec_check m2 lst)
    in rec_check lambda []
       
