(* HW1-Exercise 2*)
let sigma (a, b, f) = 
    let rec sum i total = 
        if i > b then total
        else sum (i + 1) (total + f i)
    in
    sum a 0