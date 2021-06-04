let rec iter ((n : int), (func : 'a -> 'a)) init =
  if n >= 1 then if n = 1 then func init else iter (n - 1, func) (func init)
  else func init
