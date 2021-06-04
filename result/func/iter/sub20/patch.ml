let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n = 0 then fun (__s5 : int) -> __s5
  else fun (x : int) -> f (iter (n - 1, f) x)
