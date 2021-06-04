let __s5 __s6 = __s6

let rec iter ((n : int), (f : int -> int)) (x : int) : int =
  if n = 1 then f x else if n < 1 then __s5 x else f (iter (n - 1, f) x)
