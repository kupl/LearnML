let rec iter ((n : int), (f : int -> int)) (k : int) : int =
  if n = 0 then k else iter (n - 1, f) (f k)
