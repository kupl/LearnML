(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 
  if x == y || x <= 0 || y <= 0 then 
    1 
  else 
    pascal((x-1), (y-1)) + pascal(x,(y-1))
;;
