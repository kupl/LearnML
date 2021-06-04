let rec iter ((n : int), (fn : int -> int)) (x : int) : int =
  if n <= 0 then fun __s5 -> x fn else fn (iter (n - 1, fn) x)
