let rec sigma f a b =
  if a < 0 || b < 0 then raise Invalid_argument "negative argument"
  else if a = b then f b
  else if a > b then 0
  else f a + sigma f (a + 1) b
