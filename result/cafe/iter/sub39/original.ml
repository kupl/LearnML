let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  if n = 1 || n = 0 then f x else f (iter (n - 1, f) x)
