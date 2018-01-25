(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> let rec pascal <a,b> = 
   if b=0 || b=a then 1
   else pascal <a-1,b-1> + pascal <a-1,b>;;
