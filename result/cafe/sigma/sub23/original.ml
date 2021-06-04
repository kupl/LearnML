let rec sigma (f : int -> int) (a : int) (b : int) : int =
  let rec f (n : int) : int = n in
  if a = b then f a else f a + sigma f a (b - 1)
