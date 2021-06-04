let rec sigma f a b =
  if a < b then
    let induction = f b in
    induction + sigma f a (b - 1)
  else f b
