let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = a - 1 then 0 else f b + sigma f a (b - 1)
