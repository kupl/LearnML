let rec prime_in a b =
    if a = 1 then false
    else if b = 1 then true
    else if (a mod b) = 0 then false
    else prime_in a (b-1);;

let prime : int -> bool
= fun n -> prime_in n (n-1);;