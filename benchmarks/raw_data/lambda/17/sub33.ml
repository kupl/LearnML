type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
       and var = string

let check (m : lambda) : bool = 
    let rec checkArea ((m : lambda), (vars : var list)) : bool =
        match m with
        | V n -> List.mem n vars
        | P (n, m1) -> checkArea (m1, vars @ [n]) 
        | C (m1, m2) -> (checkArea (m1, vars)) && (checkArea (m2, vars))
    in
    checkArea (m, [])

