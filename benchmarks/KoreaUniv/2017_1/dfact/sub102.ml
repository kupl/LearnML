(* problem 5*)

let rec dfact : int -> int
= fun n -> 
if n < 3 then n
else n * dfact (n-2)