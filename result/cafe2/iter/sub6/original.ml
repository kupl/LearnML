let rec iter ((n : int), (f : 'a -> 'a)) x =
  if n = 1 then f x else iter (n - 1, f) (f x)
