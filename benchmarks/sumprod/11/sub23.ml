let getm(a, b) =
  match (a,b) with
  | (1, 1) -> 1.0
  | (1, 2) -> 2.0
  | (1, 3) -> 3.0
  | (2, 1) -> 4.0
  | (2, 2) -> 5.0
  | (2, 3) -> 6.0
  | (3, 1) -> 7.0
  | (3, 2) -> 8.0
  | (3, 3) -> 9.0
  | (_, _) -> raise (Invalid_argument "argument error")

let rec sumprod(matrix, n, k) = 
  if k < 1 || n < 1 then
    raise (Invalid_argument "arg error")
  else (
    let rec row_mul(matrix, rows, cols) =
      if cols <= 0 then
        raise (Invalid_argument "error")
      else if cols == 1 then
        matrix(rows, cols)
      else
        matrix(rows, cols) *. row_mul(matrix, rows, cols-1)
    in
    let rec calculate_sumprod(matrix, rows, cols) = 
      if rows < 1 || cols < 1 then
        raise (Invalid_argument "error")
      else if rows == 1 then
        row_mul(matrix, 1, cols)
      else
        row_mul(matrix, rows, cols) +. calculate_sumprod(matrix, rows-1, cols)
    in

    calculate_sumprod(matrix, n, k)

  )

