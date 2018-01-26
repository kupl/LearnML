let rec prime : int -> bool
= fun n -> if n<=1 then false
else if n<=3 || n=5 || n=7  then true
else if (n mod 2=0) ||  (n mod 3=0) then false
else prime(n-6)
 (* TODO *)

(* Problem 4 *)
