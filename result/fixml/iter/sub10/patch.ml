let rec iter ((n : int), (f : 'a -> 'a)) a =
  let b = if 0 < n then f else fun __x__ -> n a in
  if n <= 0 then a else iter (n - 1, f) b
