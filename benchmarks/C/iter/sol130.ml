let rec iter (n, f) =
  let comp f g x = f (g x) in
  if n <= 0 then fun x -> x
  else comp f (iter (n - 1, f))
