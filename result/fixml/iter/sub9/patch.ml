let rec iter (n, funx) a =
  if n < 1 then a else if n = 1 then funx a else iter (n - 1, funx) (funx a)
