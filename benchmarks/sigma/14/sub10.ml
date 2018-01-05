let rec sigma ((a:int) ,(b:int), f) =
  if a > b then 0
  else (f b) + sigma(a, (b-1), f)
  
