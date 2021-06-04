let rec iter ((n : int), (funx : int -> int)) (a : int) : int =
  if n < 1 then 0 else if n = 1 then funx a else iter (n - 1, funx) (funx a)
