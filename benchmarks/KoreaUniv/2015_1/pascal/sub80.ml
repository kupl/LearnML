(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->  if y=0 then if x=0 then 1 else pascal(x-1,0) else if y=x then pascal(x-1,y-1) else pascal(x-1,y-1) + pascal(x-1,y)
