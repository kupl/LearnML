let rec iter ((n : int), (f : int -> int)) (r : int) : int =
  let a : int = fst (n, f) in

  let b : int -> int = snd (n, f) in
  if a > 1 then b r + iter (n - 1, f) r else if a = 1 then b r else r
