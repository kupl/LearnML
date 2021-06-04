let rec iter ((n : int), (f : int -> int)) : int -> int =
  let compose (f : int -> int) (g : int -> int) (x : int) : int = f (g x) in
  if n = 0 || n = 1 then f else compose f (iter (n - 1, f))
