(*TODO*) 
  
(* problem 5*) 
let rec dfact : int -> int 
= fun n ->
if n < 2 then 1
else n * (dfact n-2)