(* problem 5*)

let dfact : int -> int
= fun n -> 
if n<=0 then 0
else n * dfact (n-2)