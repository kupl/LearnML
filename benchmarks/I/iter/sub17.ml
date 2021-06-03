
let rec iter ((n:int), (f:int->int)) x =
  if n == 0 then 0
  else f(iter ((n-1), f) x);;
