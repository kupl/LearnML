exception Error of string 

let rec mult (matrix, i, k) =
 if k = 1 then matrix(i,k)
 else (matrix(i,k) *. (mult (matrix, i, k-1)))

let rec sumprod (matrix, n, k) =
 if (n < 1) or (k < 1) then raise (Error "n or k < 0")
 else if n = 1 then (mult (matrix, n, k))
 else (mult (matrix, n, k)) +. (sumprod (matrix, n-1, k))
