let rec iter ((n : int), (f : 'a -> 'a)) : 'a -> 'a =
  if n = 0 then fun x -> x else if n = 1 then f else fun x -> iter (n - 1, f) x
