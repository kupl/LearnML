(* ex1 *)
let rec sigma f a b =
  if a == b then
    (f a)
  else if a < b then
    (f a) + (sigma f (a+1) b)
  else
    raise (Invalid_argument "a is larger than b")
