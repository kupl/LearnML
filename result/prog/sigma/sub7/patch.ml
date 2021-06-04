let rec sigma (f : int -> int) (a : int) (b : int) : int =
  if b > b then 0
  else if b = a then f b
  else
    let __s5 : int = a + 1 in
    f a + sigma f __s5 b
