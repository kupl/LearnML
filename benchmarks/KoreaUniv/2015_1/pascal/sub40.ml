(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> 1 if y = 0 || x= y
else pascal(x-1 ,y-1) + pascal (x-1,y);;
