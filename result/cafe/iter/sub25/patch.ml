let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  if n = 0 then x
  else if n < 1 then raise Failure "ERROR"
  else f (iter (n - 1, f) x)
