exception Error of string

let rec prod (i, j, k, matrix) =
  if (j = k) then matrix(i,j)
  else matrix(i,j) *. prod(i,j+1,k,matrix)

let rec sigma (i, n, k, matrix) =
  if (i = n) then prod(i, 1, k, matrix)
  else prod(i, 1, k, matrix) +. sigma(i+1,n,k,matrix)

let rec sumprod (matrix, n, k) =
  if (n < 1 || k < 1) then raise (Error "n, k should be positive integer")
  else sigma(1,n,k,matrix)

let matrix (a,b) = 1.0
let _ =
  print_float (sumprod (matrix, 10, 10));
  print_newline ();
