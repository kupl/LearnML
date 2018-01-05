(* HW1-Exercise 3*)
let iter (n, f) = 
    let rec iter2 n f res = 
        if n<=0 then res
        else iter2 (n-1) f (fun x -> f (res x))
    in
    iter2 n f (fun x -> x)