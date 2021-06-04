let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a <= b then f b + sigma f a (b - 1) else 0
