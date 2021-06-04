let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n = 0 then f else iter (n - 1, f)


let (_ : int) = iter (4, fun (x : int) -> 6 + x) 0
