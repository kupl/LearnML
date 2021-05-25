exception Error of string
let rec iter(n,f) x = if n = 0 then
                               x                    
                           else if n <0 then 
                            raise(Error "invalid arg")
                            else
                            f (iter(n-1,f) x) 