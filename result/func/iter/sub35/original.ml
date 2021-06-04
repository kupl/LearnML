let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n = 0 then f else fun (x : int) -> iter (n - 1, f) (f x)


let (_ : int) = iter (2, fun (x : int) -> 2 + x) 0
