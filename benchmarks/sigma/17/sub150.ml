
let sigma (a, b, f) =
  let rec sum x s =
    if x > b then s
    else (sum (x + 1) (s + (f x)))
  in sum a 0

