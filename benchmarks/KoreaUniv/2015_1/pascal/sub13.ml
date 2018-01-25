(* Problem 1 *)
let pascal : int * int -> int
= fun (x,y) -> 1;;
let rec pascal (x,y)  = 
if (x < y) then raise (Failure "ERROR : Second number cannot be larger than First one.")
else if ( y=0 || x=y ) then 1 
else ((pascal (x-1,y-1)) + (pascal (x-1,y)));;