let rec f : int -> bool
= fun n -> let rec prime_Is a b =
if b = 2 then true 
else if a mod b = 0 then false
else prime_Is a (b-1)
in prime_Is n (n-1);;