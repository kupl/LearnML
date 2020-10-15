let rec sigma f x y =
  if (x > y) then 0
  else f x + sigma f (x+1) y
