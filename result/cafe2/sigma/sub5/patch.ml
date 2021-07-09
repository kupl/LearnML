let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = b - 1 then raise Invalid_argument "negative argument"
  else if a = b then f b
  else if a > b then 0
  else f a + sigma f (a + 1) b
