type metro = STATION of name
        | AREA of name * metro
        | CONNECT of metro * metro

and name = string


let checkMetro met =
        let rec checkMet met temp = 
                match met with
                | STATION( n1 ) -> List.mem n1 temp
                | AREA( n1, n2 ) -> checkMet n2 ( n1::temp )
                | CONNECT( n1, n2 ) -> checkMet n1 temp && checkMet n2 temp
        in
        checkMet met [] 

