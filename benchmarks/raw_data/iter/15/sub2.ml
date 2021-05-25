let rec iter (n, f) r =
  let a = fst (n, f) in
  let b = snd (n, f) in
    if a > 1 
      then b r + iter ((n - 1), f) r
    else if a = 1 
      then b r
    else r
