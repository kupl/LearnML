
(* problem 4*)


let rec product a b =
if a = b then a
else (product 1 b) / (product 1 a-1);; 
