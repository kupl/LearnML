let sigma f a b =
  let rec sigmaInternal n result =
    if n > b then result
    else sigmaInternal (n + 1) (result + f n)
  in sigmaInternal a 0
