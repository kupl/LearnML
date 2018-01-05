let rec __sumprod_col (matrix, i, j) =
  if j == 0 then 1.0
  else matrix (i,j) *. __sumprod_col (matrix, i, (j - 1))

let rec sumprod (matrix, i, j) =
  if i == 0 then 0.0
  else __sumprod_col (matrix, i, j) +. sumprod (matrix, (i-1), j)
