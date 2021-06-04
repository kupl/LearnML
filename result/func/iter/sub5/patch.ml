let rec iter ((n : int), (f : 'a -> 'a)) : 'a -> 'a =
  if n > 0 then fun x -> f (iter (n - 1, f) x)
  else if n = 0 then fun (__s4 : int) -> __s4
  else fun x -> iter (n - 1, f) x
