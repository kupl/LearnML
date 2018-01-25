let rec zipper : int list * int list -> int list
=fun (a,b) -> match a,b with
  | [],b -> b
  | a,[] -> a
  | (x::xs),(y::ys) -> x :: y :: (zipper (xs,ys))
  