let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b = a - 1 then
    let induction : int = f b in
    0
  else sigma f a (b - 1) + f b
