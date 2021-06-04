let rec iter ((n : int), (f : int -> int)) : int -> int =
  let rec compose (g : int -> int) (f : int -> int) (x : int) : int = g (f x) in
  if n = 0 || n = 1 then f else compose f (iter (n - 1, f))


let (_ : int) = iter (3, fun (x : int) -> 2 + x) 0
