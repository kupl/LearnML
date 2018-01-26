let rec prime : int -> bool
= fun n -> let i = n-1 in 
let rec rprime a = 
if a = 1 then true 
else if n mod a = 0 then false 
else rprime(a-1) in rprime i;;
