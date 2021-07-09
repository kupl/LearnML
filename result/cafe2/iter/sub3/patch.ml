let rec iter ((n : int), (func : 'a -> 'a)) init =
  if n = 0 then init else iter (n - 1, func) (func init)
