(* exception *)
exception Improper_input

(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
if x < 0 || y < 0 || x < y then raise Improper_input
else if y = 0 | x = y then 1
else pascal (x-1, y-1) + pascal (x-1,y)