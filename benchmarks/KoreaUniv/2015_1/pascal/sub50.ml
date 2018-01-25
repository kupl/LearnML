(* Problem 1 *)
let rec pascal : int * int -> int
=fun (x,y) -> if x = 0 || y = 0 then 1
	      else if x = y then 1 
              else (pascal(x-1,y-1) + pascal(x-1, y))

(* Problem 2 *)
let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> if a = 0 || b = 0 then 0
	      else if a = b then a 
	      else (f b + (sigma f a (b-1)))
