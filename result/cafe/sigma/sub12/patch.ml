let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a = 0 then 0
  else if a > b then 0
  else if a = b then f a
  else f a + sigma f (a + 1) b
