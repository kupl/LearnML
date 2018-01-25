(*     2013210046 조형준    *)

 (* problem 1*) 
let rec fastexpt : int -> int -> int 
= fun b n -> 
if n = 0 then 1
else if n mod 2 = 0 then fastexpt b (n/2) * fastexpt b (n/2) 
else if n mod 2 = 1 then b * fastexpt b (n-1)   
else if n = 1 then b
