let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if f a = f b then f a else f a + sigma f (a + 1) b
