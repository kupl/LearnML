type lambda = V of var
					 | P of var * lambda
					 | C of lambda * lambda
and var = string

let rec check m =  
    let rec checkArea a =
           match a with
                | (l, V var) -> List.mem var l
                | (l, C (m1, m2)) -> checkArea (l, m1) && checkArea(l,
                m2)
                | (l, P (var, lambda)) -> checkArea( var::l, lambda) 
    in
    match m with
        | V _ -> false
        | C (m1, m2) -> check m1 && check m2
        | P (var, sublambda) -> checkArea (var::[], sublambda)
