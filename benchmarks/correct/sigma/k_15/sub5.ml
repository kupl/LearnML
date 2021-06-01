let rec sigma : (int -> int) -> int -> int -> int
=fun f a b -> (* TODO *)
  if a = b then f(a)
  else (sigma(f) (a+1) b) + f(a)

