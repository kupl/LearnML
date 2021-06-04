exception Error of string

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a > b then 0 else f a + sigma f (a + 1) b
