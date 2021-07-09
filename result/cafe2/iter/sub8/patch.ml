let rec iter ((n : int), (f : int -> int)) (r : int) : int =
  let a : int = fst (n, f) in

  let b : int -> int = snd (n, f) in
  if n = 0 then r else if a = 1 then b r else iter (n - 1, f) (iter (1, f) r)
