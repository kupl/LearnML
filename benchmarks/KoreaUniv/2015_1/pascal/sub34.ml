(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 (* TODO *)

if x < y then 0
else if x == y then 1
else if x == 0 then 1
else if  y== 0 then 1
else (pascal (x-1) y) + (pascal (x-1) (y-1));;
