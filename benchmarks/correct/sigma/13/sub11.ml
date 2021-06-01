let sigma n a b =
  if a <= b then
    let rec sum (a, b, n) = 
      if a = b then n a
      else n a + sum ((a+1), b, n )
    in
    sum (a, b, n)
  else 0