let rec sumprod : (int * int -> float) * int * int -> float = fun(matrix, n, k) ->
  let rec mul(matrix, i, k) =
    if k = 1 then
      matrix(i, k)
    else
      matrix(i, k) *. mul(matrix, i, k-1)
  in
  
  if n = 1 then
    mul(matrix, 1, k)
  else
    mul(matrix, n, k) +. sumprod(matrix, n-1, k)
