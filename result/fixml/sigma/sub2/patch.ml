exception BAD

let rec sigma f a b =
  if b < b then raise BAD else if a = b then f a else f a + sigma f (a + 1) b
