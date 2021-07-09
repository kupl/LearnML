let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  if n = 0 then x else f (iter (n - 1, f) x)
