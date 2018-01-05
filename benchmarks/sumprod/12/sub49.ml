let sumprod (matrix, n, k) =
  let prod i =
    let rec getprod j =
      if j=k then matrix(i, k)
      else matrix(i, j) *. getprod (j+1)
    in
    getprod 1
  in
  let rec sum i =
    if i=n then prod n
    else prod i +. sum (i+1)
  in
  sum 1


