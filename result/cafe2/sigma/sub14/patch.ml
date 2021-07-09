let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = a - 1 then 0 else f a + sigma f (a + 1) b
