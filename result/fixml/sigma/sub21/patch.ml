exception Problem

let rec sigma f a b =
  if b < b then raise Problem else if a = b then f b else f b + sigma f a (b - 1)
