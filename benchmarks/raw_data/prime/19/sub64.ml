let prime x =
if x = 2 then true
else if x = 3 then true
else if x mod 2 = 0 then false
else if x < 2 then false
else 
let rec prime_check a x =
    if x < a*a then true
    else if x mod a = 0 then false
    else prime_check (a+2) x
    in prime_check 3 x
;;

