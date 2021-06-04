let rec sigma : (int -> int) -> int -> int -> int =
 fun f a b ->
  if a = 0 || b = 0 then 0 else if a = b then a else f b + sigma f a (b - 1)
