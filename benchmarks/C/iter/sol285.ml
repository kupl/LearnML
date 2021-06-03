let rec iter ((n: int), udf) x =
  if (n <= 0) then x
  else iter (n-1, udf) (udf x)