let rec iter (n, funx) a =
  if n < 1 then 0 else if n = 1 then funx a else iter (n - 1, funx) (funx a)
