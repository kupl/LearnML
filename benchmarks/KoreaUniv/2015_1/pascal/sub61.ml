(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
if y = 0 then 1
  else if x = y then 1
  else pascal(x-1,y-1) + pascal(x-1,y);;
