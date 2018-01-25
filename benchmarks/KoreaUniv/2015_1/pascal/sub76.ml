(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if y=0 || y=x then 1 else pascal (x-1,y-1) + pascal (x-1,y)
