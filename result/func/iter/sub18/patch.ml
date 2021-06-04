let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n < 0 then fun (b : int) -> 0
  else if n = 0 then fun (__s5 : int) -> __s5
  else fun (a : int) -> iter (n - 1, f) (f a)
