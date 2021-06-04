exception BAD

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = b - 1 then raise BAD
  else if a = b then f a
  else f a + sigma f (a + 1) b
