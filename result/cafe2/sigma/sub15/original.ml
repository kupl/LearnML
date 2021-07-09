let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a = 0 || b = 0 then 0 else if a = b then a else f b + sigma f a (b - 1)
