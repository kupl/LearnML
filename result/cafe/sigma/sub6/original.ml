let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a > b then 0 else if b = a then a else f b + sigma f a (b - 1)
