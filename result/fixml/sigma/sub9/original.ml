let rec sigma : (int -> int) -> int -> int -> int =
 fun f a b ->
  if b = 1 then 1
  else if a != 1 then sigma f 1 b - sigma f 1 a
  else f b + sigma f a (b - 1)
