let prime : int -> bool
= fun n -> let rec isnotFactor (a:int) = 
           a > (n/2) || (n mod a <> 0 && isnotFactor (a+1))
           in n >= 2 && isnotFactor 2;;