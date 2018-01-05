(* let test (x, y) = 2.0 *. float_of_int(x) +. float_of_int (y);; *)

let rec product (matrix, i, j, k) =
if j > k
then 0.0
else
  if j == k
  then
    matrix (i, j)
  else
    matrix (i, j) *. product (matrix, i, j + 1, k)
;;

let rec sum (matrix, i, n, k) =
if i > n
then 0.0
else
  if i == n
  then 0.0
  else product (matrix, i, 1, k) +. sum (matrix, i + 1, n, k)
;;

let sumprod (matrix, n, k) = sum (matrix, 1, n, k);;