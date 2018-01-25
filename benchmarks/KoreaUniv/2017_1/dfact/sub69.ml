(* problem 5*)
let rec dfact : int -> int
= fun n -> 
 match n with
 | 1 -> 1
 | 2 -> 2
 |_ -> if n mod 2 = 1 then  n * dfact (n-2)
       else n * dfact (n-2) 