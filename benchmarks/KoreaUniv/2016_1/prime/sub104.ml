let rec prime_iter a b =
 if a <= b then true
 else if a mod b = 0 then false
 else prime_iter a (b+2);;

let prime (n: int) =
 if n < 2 then false
 else if n = 2 then true
 else if n mod 2 = 0 then false
 else prime_iter n 3;;
