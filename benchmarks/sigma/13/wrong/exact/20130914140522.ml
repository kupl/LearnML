let sigma a b f = 
    let rec sub_sigma a b f = 
        if a > b then 0
        else (sub_sigma (a+1) b f) + f(a)
    in  
    sub_sigma a b f 
