exception Error of string

let rec sigma f a b =
  if a > b || a < 0 || b < 0 then raise Error "Invalid input"
  else if a = b then f b
  else f a + sigma f (a + 1) b
