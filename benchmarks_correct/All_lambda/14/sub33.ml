type lambda = V of var
        |P of var*lambda
        |C of lambda*lambda
and var = string

let check lambda = 
        let rec check2(m,nList)= 
                match m with
                |V n1 -> (List.mem n1 nList)
                |P (n1,m1) -> check2(m1,[n1]@nList)
                |C (m1,m2) -> (check2(m1,nList) && check2(m2,nList))
        in check2(lambda, [])
        
