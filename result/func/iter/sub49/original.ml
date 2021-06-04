let rec iter ((n : int), (f : int -> int)) : int -> int =
  if n > 0 then iter (n - 1, fun (x : int) -> f x) else fun (x : int) -> x


let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0
