(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1
  if y=x then 1 

  else if y=0 then 1

  else pascal(x-1,y-1)+pascal(x-1,y)

