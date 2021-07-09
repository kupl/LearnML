let abs (n : int) : int = if n < 0 then -n else n

let rec sigma (f : int -> int) (a : int) (b : int) : int =
  let n : int = abs b in
  if a <= n then sigma f a (n - 1) + f n else 0
