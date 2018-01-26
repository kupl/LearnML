let rec prime : int -> bool = fun n ->
        let rec sub_prime d
            = if n = 0 || n = 1 then false 
        		else if d * d > n then true
                else if n mod d = 0 then false
                    else sub_prime (d + 1) in
                        sub_prime 2;;
