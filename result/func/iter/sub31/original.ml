let rec iter ((n : int), (f : int -> int)) : int -> int =
  let f1 (x : int) : int = if n = 0 then f (x - 2) else f (x + 2) in
  if n = 0 then f1 else iter (n - 1, f1)
