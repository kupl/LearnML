(* Problem 1 *)
let rec sigma f a b = 
 if a=b then f a
 else f b + sigma f a (b-1);;
