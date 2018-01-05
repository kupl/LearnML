(* ex1 *)
let rec sigma (a, b, f) =
  if a == b then
    (f a)
  else if a < b then
    (f a) + (sigma ((a + 1), b, f))
  else
    raise (Invalid_argument "a is larger than b")
