let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n = 0 then fun (__s6 : int) -> __s6
  else fun (x : int) -> iter (n - 1, f) (f x)
