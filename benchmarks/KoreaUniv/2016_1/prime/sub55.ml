let rec prime2 : int -> int -> bool
= fun n p -> if p <= 1 then true else if n mod p = 0 then false else (prime2 n (p-1));;

let rec prime : int -> bool
= fun n -> prime2 n (n-1);;
