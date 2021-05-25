let rec sigma ((x : int), (y : int), udf) : int =
  if (x > y) then 0
  else (udf x) + sigma (x+1, y, udf)