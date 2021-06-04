let rec sigma f a b =
  if f a != f b then
    let induction = f b in
    induction + sigma f a (b - 1)
  else f b
