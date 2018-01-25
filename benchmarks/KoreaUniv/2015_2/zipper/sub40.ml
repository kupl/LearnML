let rec zipper : int list * int list -> int list
=fun (a,b) -> 
  match a, b with
  | [], y::[] -> [y]
  | x::[], [] -> [x]
  | x::xs, y::ys -> if (x < y) then x::zipper(xs,y::ys)
    else y::zipper(x::xs, ys)
