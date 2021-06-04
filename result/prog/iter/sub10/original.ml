let rec iter ((n : int), (f : 'a -> 'a)) a =
  let b = f a in
  if n <= 0 then a else iter (n - 1, f) b
