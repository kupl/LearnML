let rec iter(n, f) i = 
  if n <= 0 then (fun x -> x) i
  else f(iter(n-1, f) i)
