let rec sigma f x y =
  if (x > y) then 0
  else (f y) + sigma f x (y-1)
