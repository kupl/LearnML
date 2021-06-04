let abs (n : int) : int = if n < 0 then -n else n

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  let n : int = abs a in
  if b - a = 0 then f a else f a + sigma f (a + 1) b
