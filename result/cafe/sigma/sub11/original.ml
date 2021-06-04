let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a = b then b else f b + sigma f a (b - 1)
