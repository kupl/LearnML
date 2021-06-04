let rec iter ((n : int), (f : 'a -> 'a)) x =
  if n = 0 then fun (__s5 : int) -> x n else iter (n - 1, f) (f x)
