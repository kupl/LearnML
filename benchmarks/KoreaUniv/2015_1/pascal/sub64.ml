(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
match (x,y) with 
(_,0) | (1,_) -> 1
|(_,_) ->
if x=y then 1
else pascal(x-1,y-1)+pascal(x-1,y);;