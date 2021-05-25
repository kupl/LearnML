let check_mod n = 
(fun x -> x mod n=0);;


let rec prime_rec x i =
if i=x then true
else if (check_mod i x) then false
else  prime_rec x (i+1);;



let prime : int -> bool
= fun n -> prime_rec n 2;;
