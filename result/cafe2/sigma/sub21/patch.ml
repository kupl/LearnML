exception Problem

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = b - 1 then raise Problem
  else if a = b then f b
  else f b + sigma f a (b - 1)
