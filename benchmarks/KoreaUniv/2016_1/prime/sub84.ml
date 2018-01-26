let rec prime : int -> bool
= fun n -> 
        if n == 1 then true
        else 
                let rec prime_recursive = fun number divisor ->
                        if divisor == 1 then true
                        else if number mod divisor == 0 then false
                        else prime_recursive number (divisor - 1) in
                prime_recursive n (n-1);;