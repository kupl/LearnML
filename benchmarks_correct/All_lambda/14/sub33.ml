type lambda = V of var
        |P of var*lambda
        |C of lambda*lambda
and var = string

let check lambda = 
        let rec _check(m,nList)= 
                match m with
                |V n1 -> (List.mem n1 nList)
                |P (n1,m1) -> _check(m1,[n1]@nList)
                |C (m1,m2) -> (_check(m1,nList) && _check(m2,nList))
        in _check(lambda, [])
        
