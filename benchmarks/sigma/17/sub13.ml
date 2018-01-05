let rec sigma (x, y, f) =
  if (x > y) then 0
  else (f y) + sigma(x, y - 1, f)
