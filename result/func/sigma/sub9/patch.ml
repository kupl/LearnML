let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = a then f a
  else if a > b then sigma f 1 b - sigma f 1 a
  else f b + sigma f a (b - 1)
