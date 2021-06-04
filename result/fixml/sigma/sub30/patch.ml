let abs n = if n < 0 then n else n

let rec sigma : (int -> int) -> int -> int -> int =
 fun f a b ->
  let n = abs a in
  if n <= b then f n + sigma f (n + 1) b else 0
