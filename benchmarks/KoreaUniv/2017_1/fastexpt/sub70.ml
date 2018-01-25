(* problem 1*)
let fastexpt : int -> int -> int
= fun b n-> 
let rec exp b n =
if n = 0 then 1
else if n mod 2 = 1 then b * (exp b (n-1))
else let a = (exp b (n/2)) in a*a  in exp b n;; 
