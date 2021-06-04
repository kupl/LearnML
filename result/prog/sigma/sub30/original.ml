let abs (n : int) : int = if n < 0 then -n else n

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  let n : int = abs a in
  if n <= b then f n + sigma f (n + 1) b else 0
