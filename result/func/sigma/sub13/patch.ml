let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a > b then 0 else sigma f a (b - 1) + f b
