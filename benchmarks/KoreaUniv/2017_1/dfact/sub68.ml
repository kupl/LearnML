exception Problem

(*problem 5*)

let rec dfact : int -> int
= fun n->
if (0<=n && n<=1) then 1
else if n>1 then n*dfact (n-2)
else raise Problem
