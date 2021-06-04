let rec iter ((n : int), (fn : 'a -> 'a)) x =
  if n = 0 then fun (__s5 : int) -> x n else iter (n - 1, fn) (fn x)
