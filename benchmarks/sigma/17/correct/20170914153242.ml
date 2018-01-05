let rec sigma(a, b, f) = (
  let rec sigma2 a b f r = (
    if a > b then r else (sigma2 (a+1) b f (r+f(a)))
  ) in (sigma2 a b f 0)
)
