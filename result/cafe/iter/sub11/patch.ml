let rec iter ((n : int), (fn : 'a -> 'a)) x =
  if n = 0 then x else iter (n - 1, fn) (fn x)
