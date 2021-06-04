let abs n = if n < 0 then -n else n

let rec sigma : (int -> int) -> int -> int -> int =
 fun f a b ->
  let n = abs b in
  if a <= n then sigma f a (n - 1) + f n else 0
