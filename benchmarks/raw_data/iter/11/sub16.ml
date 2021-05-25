
let rec iter (n, f) i = 
    if n==0 then i
    else f (iter (n-1,f) i)
