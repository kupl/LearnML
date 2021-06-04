let rec iter ((n : int), (f : 'a -> 'a)) : 'a -> 'a =
  if n != 0 then fun x -> iter (n - 1, f) (f x)
  else if n = 0 then fun __s4 -> __s4
  else fun x -> iter (n - 1, f) x
