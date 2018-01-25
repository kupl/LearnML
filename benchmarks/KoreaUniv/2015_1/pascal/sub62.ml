(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) ->
if y>x then raise (Failure "wrong input location")
  else if y=0 then 1
  else if y=x then 1
  else pascal(x-1,y-1) + pascal(x-1,y);;
