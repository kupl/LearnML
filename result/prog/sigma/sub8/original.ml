let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = a then a else f b + sigma f a (b - 1)
