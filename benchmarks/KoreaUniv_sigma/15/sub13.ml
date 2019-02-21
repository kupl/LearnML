(* Problem 1 *)

let rec sigma f x y = if (x < y) then (f x + (sigma f (x+1) y))
else f x;;
