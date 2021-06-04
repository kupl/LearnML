let rec iter ((n : int), (f : int -> int)) x : int =
  if n > 0 then iter (n - 1, f) (f x)
  else if n = 0 then x
  else raise Failure "Exception(Template)"
