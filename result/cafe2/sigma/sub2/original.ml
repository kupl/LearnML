exception BAD

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a < 0 || b < 0 || a > b then raise BAD
  else if a = b then f a
  else f a + sigma f (a + 1) b
