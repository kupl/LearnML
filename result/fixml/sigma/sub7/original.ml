let rec sigma f a b =
  match b with
  | 0 -> 0
  | _ -> if b < a then 0 else if b = a then f a else f a + sigma f (a + 1) b
