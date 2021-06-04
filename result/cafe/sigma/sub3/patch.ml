exception Error of string

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = b - 1 then raise Error "Invalid input"
  else if a = b then f b
  else f a + sigma f (a + 1) b
