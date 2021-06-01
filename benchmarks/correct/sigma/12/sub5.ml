let rec sigma f a b =
  if b = a then f b
  else if b > a then f b + sigma f a (b-1)
  else 0