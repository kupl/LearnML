let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n = 1 then f else fun (x : int) -> iter (n - 1, f) (f x)
