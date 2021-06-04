let __s5 __s6 = __s6

let rec iter ((n : int), (func : 'a -> 'a)) init =
  if n = 0 then __s5 init
  else if init = 1 then func n
  else iter (n - 1, func) (func init)
