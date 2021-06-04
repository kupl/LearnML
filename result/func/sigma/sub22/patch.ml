let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if a = b then
    let induction : int = f b in
    f b
  else sigma f a (b - 1) + sigma f b b
