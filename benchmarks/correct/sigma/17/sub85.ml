let rec sigma f a b =
  if (a > b) then 0
  else match (a, b, f) with
       | (a, b, f) -> if (a = b) then f a
                      else f a + sigma f (a+1) b
