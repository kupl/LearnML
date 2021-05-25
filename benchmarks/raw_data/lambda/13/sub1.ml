type lambda = V of var
        | P of var * lambda
        | C of lambda * lambda

and var = string


let check met =
        let rec checkMet met temp = 
                match met with
                | V( n1 ) -> List.mem n1 temp
                | P( n1, n2 ) -> checkMet n2 ( n1::temp )
                | C( n1, n2 ) -> checkMet n1 temp && checkMet n2 temp
        in
        checkMet met [] 

