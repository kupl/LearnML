let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if f a != f b then
    let induction : int = f b in
    induction + sigma f a (b - 1)
  else f b
