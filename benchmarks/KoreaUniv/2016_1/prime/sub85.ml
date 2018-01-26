let rec primeaux : int -> int -> bool 
= fun n1 n2 ->
if n1 = n2 then true
else if n1 mod n2 = 0 then false 
else primeaux n1 (n2 + 1) 

let prime : int -> bool
= fun n ->
if n < 1 then false
else if n = 1 then true
else primeaux n 2
