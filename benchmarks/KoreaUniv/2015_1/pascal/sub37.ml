(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with
|(0,0) -> 1
|(x,y) -> if (x=y) then 1
|(x,y) -> if (y=0) then 1
|else pascal(x-1,y-1) + pascal(x-1,y);;