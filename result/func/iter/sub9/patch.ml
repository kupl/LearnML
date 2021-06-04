let rec iter ((n : int), (funx : int -> int)) (a : int) : int =
  if n > 0 then funx (iter (n - 1, funx) a)
  else if n = 0 then a
  else iter (n - 1, funx) (funx a)
