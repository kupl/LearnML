let rec iter(n, f) k = 
  if n = 0 then
    (fun x -> x) k
  else
    f(iter(n-1, f) k)
