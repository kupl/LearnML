let rec is_prime : int * int -> bool
= fun (n1, n2) -> if (n2 = 1) then true else if (n1 mod n2 = 0) then false  else is_prime (n1, n2-1) 

let rec prime : int -> bool
= fun n -> if n = 0 then false else if n = 1 then true else if n = 2 then true else is_prime(n, n-1)
