let rec sigma ((a: int), (b: int), (f: (int -> int))): int = 
  if (a > b) then 0
  else match (a, b, f) with
       | (a, b, f) -> if (a = b) then f a
                      else f a + sigma (a+1, b, f)
