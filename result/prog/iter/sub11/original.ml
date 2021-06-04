let rec iter ((n : int), (fn : 'a -> 'a)) x =
  if n > 1 then iter (n - 1, fn) (fn x) else fn x
