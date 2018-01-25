let dfact : int -> int
= fun n ->
 
let even n = product (fun x -> 2*x) 1 (n/2)
 
in
 
let odd n = product (fun x -> 2*x-1) 1 ((n+1)/2)
 
in
 
if n < 0 then raise (Failure "n is negative value")
else if n mod 2 = 0 then even n
else odd n