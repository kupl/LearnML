(* problem 2*)

exception Problem

let rec findPrime : int -> int -> int
= fun n p -> if (p*p)>n then n
             else if n mod p = 0 then p
             else (findPrime n (p+1))

let smallest_divisor : int -> int
= fun n -> if (n<=0) then raise Problem
           else if (n=1) then 1
           else if n mod 2 = 0 then 2
           else findPrime n 3