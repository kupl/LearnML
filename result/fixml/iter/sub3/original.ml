let rec iter (n, func) init =
  if n >= 1 then if n = 1 then func init else iter (n - 1, func) (func init)
  else func init
