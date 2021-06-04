exception Problem

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = a then f a else if a = b then f b else f b + sigma f a (b - 1)
