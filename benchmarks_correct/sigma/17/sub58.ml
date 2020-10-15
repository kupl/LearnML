let rec sigma udf x y =
  if (x > y) then 0
  else (udf x) + sigma udf (x+1) y